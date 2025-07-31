import tkinter as tk
from tkinter import filedialog, scrolledtext
from langchain_community.vectorstores import Chroma
from langchain_huggingface import HuggingFaceEmbeddings
from langchain.docstore.document import Document
from llama_cpp import Llama
import tiktoken

# === CONFIGURACIÓN ===
LLM_PATH              = r"D:\llm\model.gguf"
VECTOR_DIR_1          = r"E:\IA_RAPID\vectorstore"
VECTOR_DIR_2          = r"E:\IA_RAPID\vectorstore_mpnet"
EMBEDDINGS_PATH_1     = r"E:\IA_RAPID\embeddings\multilingual-e5-large"
EMBEDDINGS_PATH_2     = r"E:\IA_RAPID\embeddings\all-mpnet-base-v2"

MAX_TOKENS_POR_FRAGMENTO = 2500
MAX_TOKENS_RESPUESTA     = 1024
LLM_N_CTX                = 4096  # Límite de contexto del modelo
TOKEN_MARGIN             = 200   # Reserva para prompts internos

ENCODING = tiktoken.get_encoding("cl100k_base")

# === MODELOS ===
def cargar_llm(path):
    return Llama(
        model_path=path,
        n_ctx=LLM_N_CTX,
        n_threads=6,
        n_gpu_layers=20,
        chat_format="chatml"
    )

def cargar_vectores():
    e1 = HuggingFaceEmbeddings(model_name=EMBEDDINGS_PATH_1)
    e2 = HuggingFaceEmbeddings(model_name=EMBEDDINGS_PATH_2)
    db1 = Chroma(persist_directory=VECTOR_DIR_1, embedding_function=e1)
    db2 = Chroma(persist_directory=VECTOR_DIR_2, embedding_function=e2)
    return db1, db2

# === UTILIDADES ===
def contar_tokens(texto: str) -> int:
    return len(ENCODING.encode(texto))

def dividir_en_fragmentos_por_tokens(texto: str, max_tokens: int = MAX_TOKENS_POR_FRAGMENTO):
    lineas = texto.splitlines()
    fragmentos, actual = [], ""
    for linea in lineas:
        prueba = (actual + "\n" + linea).strip()
        if contar_tokens(prueba) > max_tokens:
            if actual:
                fragmentos.append(actual.strip())
            actual = linea
        else:
            actual = prueba
    if actual:
        fragmentos.append(actual.strip())
    return fragmentos

def agregar_documento(db, texto, metadatos=None):
    doc = Document(page_content=texto, metadata=metadatos or {})
    db.add_documents([doc])
    db.persist()

def preguntar_con_razonamiento(llm, pregunta: str, contexto: str = "") -> str:
    system_msg = {
        "role": "system",
        "content": "Actúa como un experto en robótica industrial ABB, análisis de fallos y sistemas de control."
    }
    user_content = (
        f"Pregunta: {pregunta}\n\n"
        f"Contexto:\n{contexto}\n\n"
        "Por favor, razona paso a paso y ofrece diagnóstico técnico y recomendaciones."
    )

    # Medir tokens de entrada + respuesta + margen, usando LLM_N_CTX
    tokens_sys = contar_tokens(system_msg["content"])
    tokens_usr = contar_tokens(user_content)
    if tokens_sys + tokens_usr + MAX_TOKENS_RESPUESTA + TOKEN_MARGIN > LLM_N_CTX:
        # recortamos contexto
        espacio_disponible = LLM_N_CTX - tokens_sys - MAX_TOKENS_RESPUESTA - TOKEN_MARGIN
        ctx_tokens = ENCODING.encode(contexto)[:espacio_disponible]
        contexto_recortado = ENCODING.decode(ctx_tokens)
        user_content = (
            f"Pregunta: {pregunta}\n\n"
            f"Contexto:\n{contexto_recortado}\n\n"
            "Por favor, razona paso a paso."
        )

    resp = llm.create_chat_completion(
        messages=[system_msg, {"role": "user", "content": user_content}],
        max_tokens=MAX_TOKENS_RESPUESTA,
        temperature=0.5
    )
    return resp["choices"][0]["message"]["content"].strip()

def resumir_fragmento(llm, texto: str) -> str:
    prompt = (
        "Resume este fragmento del log en un máximo de 300 tokens, "
        "conservando solo detalles técnicos críticos (errores, valores anómalos, códigos)."
    )
    return preguntar_con_razonamiento(llm, prompt, texto)

# === INTERFAZ ===
class AsistenteApp:
    def __init__(self, master):
        self.master = master
        master.title("🤖 Asistente IA RAPID + Análisis de Logs")
        master.geometry("950x700")

        self.llm = cargar_llm(LLM_PATH)
        self.db1, self.db2 = cargar_vectores()

        tk.Label(master, text="Haz tu pregunta sobre RAPID, ABB o sube un LOG para analizar:")\
          .pack(pady=5)

        self.entry = tk.Entry(master, width=100)
        self.entry.pack(padx=10, pady=5)
        self.entry.bind("<Return>", self.consultar)

        tk.Button(master, text="Consultar", command=self.consultar).pack(pady=5)
        tk.Button(master, text="💾 Corregir y aprender", command=self.corregir).pack(pady=5)
        tk.Button(master, text="📂 Analizar archivo LOG", command=self.analizar_log).pack(pady=5)

        self.salida = scrolledtext.ScrolledText(master, wrap=tk.WORD, height=30)
        self.salida.pack(padx=10, pady=5, fill=tk.BOTH, expand=True)

    def consultar(self, event=None):
        query = self.entry.get().strip()
        self.salida.delete(1.0, tk.END)
        if not query:
            self.salida.insert(tk.END, "⚠️ Introduce una pregunta.\n")
            return

        # Solo k=1 para no exceder contexto
        r1 = self.db1.similarity_search(query, k=1)
        r2 = self.db2.similarity_search(query, k=1)
        contexto = "\n".join([doc.page_content for doc in r1 + r2])

        respuesta = preguntar_con_razonamiento(self.llm, query, contexto)
        self.salida.insert(tk.END, "\n🧠 Respuesta IA combinada:\n" + respuesta + "\n")

        with open("interacciones_guardadas.txt", "a", encoding="utf-8") as f:
            f.write(f"Pregunta: {query}\nRespuesta:\n{respuesta}\n====\n")

    def corregir(self):
        texto = self.salida.get("1.0", tk.END).strip()
        query = self.entry.get().strip()
        if texto and query:
            agregar_documento(
                self.db1,
                texto,
                {"fuente": "correccion_manual", "pregunta": query}
            )
            self.salida.insert(tk.END, "\n✅ Corrección añadida al vectorstore.\n")

    def analizar_log(self):
        ruta = filedialog.askopenfilename(
            title="Selecciona archivo de log",
            filetypes=[("Archivos TXT", "*.txt")]
        )
        if not ruta:
            return

        self.salida.delete(1.0, tk.END)
        self.salida.insert(tk.END, f"📂 Analizando archivo: {ruta}\n")

        try:
            with open(ruta, encoding="utf-8", errors="ignore") as f:
                texto = f.read()

            # Filtrar líneas clave
            claves = [
                "Too high CPU", "Auto Stop open", "Jog in wrong direction",
                "Motors On rejected", "Program stopped", "Program restarted",
                "Safety guard stop", "Motion supervision", "DC Link Voltage Too Low"
            ]
            lineas = [ln for ln in texto.splitlines() if any(k in ln for k in claves)]
            if not lineas:
                lineas = texto.splitlines()

            texto_filtrado = "\n".join(lineas)
            fragmentos = dividir_en_fragmentos_por_tokens(texto_filtrado)

            # Resumir cada fragmento
            self.salida.insert(tk.END, "\n🔍 Resumiendo fragmentos...\n")
            resúmenes = []
            for i, frag in enumerate(fragmentos, 1):
                self.salida.insert(
                    tk.END,
                    f"  • Resumiendo fragmento {i}/{len(fragmentos)}...\n"
                )
                resúmenes.append(resumir_fragmento(self.llm, frag))

            # Análisis unificado sobre resúmenes
            resumen_global = "\n".join(resúmenes)
            síntesis = preguntar_con_razonamiento(
                self.llm,
                "Basándote en estos resúmenes, ofrece un análisis unificado "
                "de los problemas técnicos y proporciona recomendaciones.",
                resumen_global
            )

            self.salida.insert(tk.END, "\n🧩 Análisis final:\n" + síntesis + "\n")
            with open("resumen_log_final.txt", "w", encoding="utf-8") as f:
                f.write(síntesis)

        except Exception as e:
            self.salida.insert(
                tk.END,
                f"\n❌ Error al analizar el log: {e}\n"
            )

# === EJECUCIÓN ===
if __name__ == "__main__":
    root = tk.Tk()
    app = AsistenteApp(root)
    root.mainloop()