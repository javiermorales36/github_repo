import os
import json
import shutil
import warnings
from pathlib import Path
from langchain_community.embeddings import HuggingFaceEmbeddings
from langchain_community.vectorstores import Chroma
from langchain_community.document_loaders import PyMuPDFLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.schema.document import Document
import torch

# === CONFIGURACIÓN ===
RAICES = [r"E:\OneDrive - ABB", r"E:\IA_RAPID\docs"]
VECTOR_DIR = r"E:\IA_RAPID\vectorstore"
EMBEDDINGS_PATH = r"E:\IA_RAPID\embeddings\multilingual-e5-large"
INDEX_LOG = os.path.join(VECTOR_DIR, "indexed_files.json")
CHECKPOINT_LOG = os.path.join(VECTOR_DIR, "checkpoint_files.json")
MAX_CHUNK = 500

# === UTILS ===
def cargar_logs(path):
    if os.path.exists(path):
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    return {}

def guardar_logs(path, data):
    if os.path.exists(path):
        shutil.copy(path, path + ".backup")
    with open(path, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)

# === EMBEDDINGS CON GPU ===
def crear_vectorstore():
    device = "cuda" if torch.cuda.is_available() else "cpu"
    print(f"🔧 Usando embeddings en: {device}")
    embeddings = HuggingFaceEmbeddings(
        model_name=EMBEDDINGS_PATH,
        model_kwargs={"device": device}
    )
    if os.path.exists(VECTOR_DIR):
        return Chroma(persist_directory=VECTOR_DIR, embedding_function=embeddings)
    else:
        return Chroma.from_documents([], embedding=embeddings, persist_directory=VECTOR_DIR)

def dividir_documentos(documentos):
    splitter = RecursiveCharacterTextSplitter(chunk_size=500, chunk_overlap=50)
    return splitter.split_documents(documentos)

def procesar_archivo(archivo):
    if archivo.suffix.lower() == ".pdf":
        loader = PyMuPDFLoader(str(archivo))
        docs = loader.load()
        print(f"📄 Procesando PDF: {archivo} ({len(docs)} páginas)")
    else:
        with open(archivo, "r", encoding="utf-8", errors="ignore") as f:
            texto = f.read()
        docs = [Document(page_content=texto, metadata={"source": str(archivo)})]
        print(f"📁 Procesando texto: {archivo}")
    return dividir_documentos(docs)

# === PROCESO PRINCIPAL ===
if __name__ == "__main__":
    warnings.filterwarnings("ignore")

    log_indexado = cargar_logs(INDEX_LOG)
    checkpoint = cargar_logs(CHECKPOINT_LOG)
    db = crear_vectorstore()
    nuevos_logs = {}

    for raiz in RAICES:
        for archivo in Path(raiz).rglob("*"):
            if archivo.suffix.lower() not in [".pdf", ".txt", ".cfg", ".mod", ".modx"]:
                continue

            archivo_str = str(archivo)
            mod_time = str(os.path.getmtime(archivo))

            if archivo_str in log_indexado and log_indexado[archivo_str] == mod_time:
                continue
            if archivo_str in checkpoint:
                continue

            try:
                chunks = procesar_archivo(archivo)
                for i in range(0, len(chunks), MAX_CHUNK):
                    db.add_documents(chunks[i:i + MAX_CHUNK])
                db.persist()
                nuevos_logs[archivo_str] = mod_time
                checkpoint[archivo_str] = True
                guardar_logs(CHECKPOINT_LOG, checkpoint)
            except Exception as e:
                print(f"❌ Error procesando {archivo}: {e}")
                continue

    log_indexado.update(nuevos_logs)
    guardar_logs(INDEX_LOG, log_indexado)

    print("✅ Vectorización completa.")
