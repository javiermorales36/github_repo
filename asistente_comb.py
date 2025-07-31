import os
import json
import shutil
import warnings
import subprocess
import tempfile
import glob
import hashlib
from pathlib import Path
from bs4 import BeautifulSoup

import torch
from langchain_community.embeddings import HuggingFaceEmbeddings
from langchain_community.vectorstores import Chroma
from langchain_community.document_loaders import PyMuPDFLoader
from langchain.schema.document import Document
from langchain.text_splitter import RecursiveCharacterTextSplitter

# === CONFIGURACIÓN ===
RAICES = [r"E:\IA_RAPID\docs", r"E:\OneDrive - ABB"]
MAX_CHUNK = 500

# === RUTAS EMBEDDINGS Y VECTORSTORES ===
EMBEDDINGS_PATH_1 = r"E:\IA_RAPID\embeddings\multilingual-e5-large"
VECTOR_DIR_1 = r"E:\IA_RAPID\vectorstore"
INDEX_LOG_1 = os.path.join(VECTOR_DIR_1, "indexed_files.json")
CHECKPOINT_LOG_1 = os.path.join(VECTOR_DIR_1, "checkpoint_files.json")
HASH_LOG_1 = os.path.join(VECTOR_DIR_1, "hashed_chunks.json")

EMBEDDINGS_PATH_2 = r"E:\IA_RAPID\embeddings\all-mpnet-base-v2"
VECTOR_DIR_2 = r"E:\IA_RAPID\vectorstore_mpnet"
INDEX_LOG_2 = os.path.join(VECTOR_DIR_2, "indexed_files.json")
CHECKPOINT_LOG_2 = os.path.join(VECTOR_DIR_2, "checkpoint_files.json")
HASH_LOG_2 = os.path.join(VECTOR_DIR_2, "hashed_chunks.json")

# === FUNCIONES AUXILIARES ===
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

def limpiar_html(texto):
    return BeautifulSoup(texto, "html.parser").get_text(separator="\n").strip()

def crear_vectorstore(embed_path, vector_dir):
    device = "cuda" if torch.cuda.is_available() else "cpu"
    print(f"🔧 Usando embeddings en: {device} ({embed_path})")
    embeddings = HuggingFaceEmbeddings(
        model_name=embed_path,
        model_kwargs={"device": device}
    )
    os.makedirs(vector_dir, exist_ok=True)
    return Chroma(persist_directory=vector_dir, embedding_function=embeddings)

def dividir_documentos(documentos):
    splitter = RecursiveCharacterTextSplitter(
        chunk_size=500,
        chunk_overlap=50,
        separators=["\n\n", "\n", ".", " "]
    )
    return splitter.split_documents(documentos)

def extraer_texto_de_chm(chm_path):
    try:
        with tempfile.TemporaryDirectory() as temp_dir:
            comando = f'hh.exe -decompile "{temp_dir}" "{chm_path}"'
            resultado = subprocess.run(comando, shell=True, capture_output=True)
            if resultado.returncode != 0:
                print(f"❌ Fallo al extraer {chm_path}: {resultado.stderr.decode()}")
                return ""
            texto_total = ""
            for html_file in glob.glob(os.path.join(temp_dir, "**", "*.htm*"), recursive=True):
                try:
                    with open(html_file, "r", encoding="utf-8", errors="ignore") as f:
                        html = f.read()
                        texto_total += limpiar_html(html) + "\n"
                except:
                    continue
            return texto_total
    except Exception as e:
        print(f"❌ Error interno al leer CHM {chm_path}: {e}")
        return ""

def procesar_archivo(archivo: Path):
    nombre_base = archivo.stem
    ruta = str(archivo)

    if archivo.suffix.lower() == ".pdf":
        loader = PyMuPDFLoader(ruta)
        docs = loader.load()
        print(f"📄 Procesando PDF: {ruta} ({len(docs)} páginas)")
    elif archivo.suffix.lower() == ".chm":
        texto = extraer_texto_de_chm(archivo)
        docs = [Document(page_content=texto, metadata={"source": ruta, "tipo": "chm", "nombre": nombre_base})] if texto.strip() else []
        print(f"📘 CHM procesado: {ruta} ({'OK' if docs else 'vacío'})")
    elif archivo.suffix.lower() in [".html", ".htm"]:
        with open(archivo, "r", encoding="utf-8", errors="ignore") as f:
            texto = limpiar_html(f.read())
        docs = [Document(page_content=texto, metadata={"source": ruta, "tipo": "html", "nombre": nombre_base})]
        print(f"🌐 HTML limpio: {ruta}")
    else:
        with open(archivo, "r", encoding="utf-8", errors="ignore") as f:
            texto = f.read()
        docs = [Document(page_content=texto, metadata={"source": ruta, "tipo": "texto", "nombre": nombre_base})]
        print(f"📁 Texto plano: {ruta}")

    return dividir_documentos(docs)

def hash_chunk(texto):
    return hashlib.sha256(texto.strip().encode("utf-8")).hexdigest()

# === EJECUCIÓN PRINCIPAL ===
if __name__ == "__main__":
    warnings.filterwarnings("ignore")

    log_1, chk_1, hash_1 = cargar_logs(INDEX_LOG_1), cargar_logs(CHECKPOINT_LOG_1), cargar_logs(HASH_LOG_1)
    log_2, chk_2, hash_2 = cargar_logs(INDEX_LOG_2), cargar_logs(CHECKPOINT_LOG_2), cargar_logs(HASH_LOG_2)

    db_1 = crear_vectorstore(EMBEDDINGS_PATH_1, VECTOR_DIR_1)
    db_2 = crear_vectorstore(EMBEDDINGS_PATH_2, VECTOR_DIR_2)

    nuevos_1, nuevos_2 = {}, {}

    for raiz in RAICES:
        for archivo in Path(raiz).rglob("*.*"):
            if archivo.suffix.lower() not in [".pdf", ".txt", ".cfg", ".mod", ".modx", ".chm", ".html", ".htm"]:
                continue

            archivo_str = str(archivo)
            mod_time = str(os.path.getmtime(archivo))

            necesita_1 = archivo_str not in log_1 or log_1[archivo_str] != mod_time
            necesita_2 = archivo_str not in log_2 or log_2[archivo_str] != mod_time

            if not (necesita_1 or necesita_2):
                continue

            try:
                chunks = procesar_archivo(archivo)

                nuevos_chunks_1 = [chunk for chunk in chunks if hash_chunk(chunk.page_content) not in hash_1]
                nuevos_chunks_2 = [chunk for chunk in chunks if hash_chunk(chunk.page_content) not in hash_2]

                if necesita_1 and nuevos_chunks_1 and archivo_str not in chk_1:
                    for c in nuevos_chunks_1:
                        hash_1[hash_chunk(c.page_content)] = archivo_str
                    for i in range(0, len(nuevos_chunks_1), MAX_CHUNK):
                        db_1.add_documents(nuevos_chunks_1[i:i + MAX_CHUNK])
                    db_1.persist()
                    nuevos_1[archivo_str] = mod_time
                    chk_1[archivo_str] = True
                    guardar_logs(CHECKPOINT_LOG_1, chk_1)
                    guardar_logs(HASH_LOG_1, hash_1)

                if necesita_2 and nuevos_chunks_2 and archivo_str not in chk_2:
                    for c in nuevos_chunks_2:
                        hash_2[hash_chunk(c.page_content)] = archivo_str
                    for i in range(0, len(nuevos_chunks_2), MAX_CHUNK):
                        db_2.add_documents(nuevos_chunks_2[i:i + MAX_CHUNK])
                    db_2.persist()
                    nuevos_2[archivo_str] = mod_time
                    chk_2[archivo_str] = True
                    guardar_logs(CHECKPOINT_LOG_2, chk_2)
                    guardar_logs(HASH_LOG_2, hash_2)

            except Exception as e:
                print(f"❌ Error procesando {archivo}: {e}")
                continue

    log_1.update(nuevos_1)
    guardar_logs(INDEX_LOG_1, log_1)

    log_2.update(nuevos_2)
    guardar_logs(INDEX_LOG_2, log_2)

    print("✅ Vectorización completa y deduplicada.")
