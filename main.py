import os

ROOT_DIR = os.getcwd()
PHRASE = "Zone.Identifier"
DELETE = True  # <- ustaw na True żeby usuwać

matched_files = []
deleted_files = []

for root, dirs, files in os.walk(ROOT_DIR):
    for file in files:
        if PHRASE in file:
            file_path = os.path.join(root, file)
            matched_files.append(file_path)

            print(f"[FOUND] {file_path}")

            if DELETE:
                try:
                    os.remove(file_path)
                    deleted_files.append(file_path)
                    print(f"[DELETED] {file_path}")
                except Exception as e:
                    print(f"[ERROR] {file_path} -> {e}")

print("\n--- PODSUMOWANIE ---")
print(f"Znalezione: {len(matched_files)}")
print(f"Usunięte: {len(deleted_files)}")