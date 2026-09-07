import json
import os

def convert_to_sharegpt(data):
    """
    Converts to ShareGPT format:
    [{"conversations": [{"from": "human", "value": "..."}, {"from": "gpt", "value": "..."}]}]
    """
    sharegpt_data = []
    for session in data:
        convs = []
        for msg in session.get('messages', []):
            role = "human" if msg.get('role') == 'user' else "gpt"
            convs.append({"from": role, "value": msg.get('content', '')})
        sharegpt_data.append({"conversations": convs})
    return sharegpt_data

def convert_to_llama(data):
    """
    Converts to LLaMa ChatML style:
    <|im_start|>user\n...\n<|im_end|>\n<|im_start|>assistant\n...\n<|im_end|>
    """
    llama_outputs = []
    for session in data:
        text = ""
        for msg in session.get('messages', []):
            role = "user" if msg.get('role') == 'user' else "assistant"
            text += f"<|im_start|>{role}\n{msg.get('content', '')}\n<|im_end|>\n"
        llama_outputs.append(text)
    return llama_outputs

def convert_to_lisp(data):
    """
    Converts to Mark's memory format:
    ((q1 a1 nil 1.0) (q2 a2 nil 1.0) ...)
    """
    memory_entries = []
    for session in data:
        msgs = session.get('messages', [])
        for i in range(len(msgs) - 1):
            # We only take pairs: User -> Assistant
            if msgs[i].get('role') == 'user' and msgs[i+1].get('role') != 'user':
                q = msgs[i].get('content', '').replace('"', '\\"')
                a = msgs[i+1].get('content', '').replace('"', '\\"')
                # Format: (question answer call confidence)
                memory_entries.append(f'("{q}" "{a}" nil 1.0)')
    
    # Wrap everything in one big list
    return f"({ ' '.join(memory_entries) })"

def main():
    input_file = 'tg_logs.json'  # Default input file
    if not os.path.exists(input_file):
        print(f"Error: {input_file} not found. Please place your Telegram logs in this file.")
        return

    with open(input_file, 'r', encoding='utf-8') as f:
        try:
            data = json.load(f)
        except json.JSONDecodeError:
            print("Error: Failed to decode JSON.")
            return

    # ShareGPT
    with open('dataset_sharegpt.json', 'w', encoding='utf-8') as f:
        json.dump(convert_to_sharegpt(data), f, ensure_ascii=False, indent=2)
    
    # LLaMa
    with open('dataset_llama.txt', 'w', encoding='utf-8') as f:
        f.write('\n\n'.join(convert_to_llama(data)))
        
    # Lisp (Mark's memory format)
    with open('dataset_lisp.lisp', 'w', encoding='utf-8') as f:
        f.write(convert_to_lisp(data))

    print("✅ Conversion complete!")
    print("- dataset_sharegpt.json")
    print("- dataset_llama.txt")
    print("- dataset_lisp.lisp (Ready for Mark!)")

if __name__ == "__main__":
    main()
