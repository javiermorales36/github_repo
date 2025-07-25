from langchain_community.vectorstores import Chroma
from langchain_huggingface import HuggingFaceEmbeddings
from llama_cpp import Llama
import os

VECTOR_DIR = r"E:\IA_RAPID\vectorstore"
LLM_PATH = r"D:\llm\model.gguf"
EMBEDDINGS_PATH = r"E:\IA_RAPID\embeddings\multilingual-e5-large"

def cargar_llm(ruta_modelo):
    print("🚀 Cargando modelo Qwen2.5...")
    return Llama(
        model_path=ruta_modelo,
        n_ctx=2048,
        n_threads=6,
        n_gpu_layers=20,
        chat_format="chatml"  # para Qwen2.5
    )

def preguntar_llm(llm, pregunta):
    mensajes = [
        {"role": "system", "content": "Actúa como un experto en robótica ABB y programación RAPID."},
        {"role": "user", "content": pregunta}
    ]
    respuesta = llm.create_chat_completion(messages=mensajes, max_tokens=512, temperature=0.7)
    return respuesta["choices"][0]["message"]["content"].strip()

def buscar_por_codigo_directo(db, codigo):
    try:
        data = db.get()
        for doc, meta in zip(data["documents"], data["metadatas"]):
            if doc.strip().startswith(str(codigo)):
                return doc
    except Exception as e:
        print(f"❌ Error al buscar código exacto: {e}")
    return None

if __name__ == "__main__":
    print("🔍 Cargando índice vectorial...")
    embeddings = HuggingFaceEmbeddings(model_name=EMBEDDINGS_PATH)
    db = Chroma(persist_directory=VECTOR_DIR, embedding_function=embeddings)

    llm = cargar_llm(LLM_PATH)

    print("\n🤖 Pregunta sobre cualquier documento indexado (escribe 'salir'):")
    while True:
        query = input("\n🧠 Tu pregunta: ")
        if query.lower() in ["salir", "exit", "quit"]:
            break

        # Intentar detectar código RAPID en la pregunta (5 cifras)
        codigos = [int(s) for s in query.split() if s.isdigit() and len(s) == 5]
        resultado_directo = None
        if codigos:
            resultado_directo = buscar_por_codigo_directo(db, codigos[0])

        if resultado_directo:
            print("\n📘 Respuesta exacta encontrada:")
            print("-", resultado_directo.strip())
        else:
            resultados = db.similarity_search(query, k=3)
            if resultados:
                print("\n📘 Respuesta basada en documentos:")
                for r in resultados:
                    print("-", r.page_content.strip())
            else:
                print("\n🤖 Respuesta generada por IA local:")
                respuesta = preguntar_llm(llm, query)
                print(respuesta)
