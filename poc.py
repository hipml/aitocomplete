import requests
import json
import sys

def stream_llm_response(prompt, model="llama3.2"):
    url = "http://localhost:11434/api/generate"
    headers = {
        'Content-Type': 'application/json'
    }
    data = {
        "model": model,
        "prompt": prompt
    }

    try:
        with requests.post(url, json=data, headers=headers, stream=True) as response:
            if response.status_code != 200:
                print(f"Error: Got status code {response.status_code}")
                print(f"Response content: {response.text}")
                return None

            full_response = ""
            for line in response.iter_lines():
                if line:
                    chunk = json.loads(line.decode('utf-8'))
                    
                    if 'response' in chunk:
                        sys.stdout.write(chunk['response'])
                        sys.stdout.flush()
                        full_response += chunk['response']
                    
                    if chunk.get('done', False):
                        print('\n\n--- Generation Stats ---')
                        for key, value in chunk.items():
                            if key not in ['response', 'model']:
                                print(f"{key}: {value}")
            
            return full_response

    except requests.exceptions.RequestException as e:
        print(f"Request failed: {e}")
        return None

if __name__ == "__main__":
    prompt = "Why is the sky blue?"
    print(f"\nPrompt: {prompt}\n")
    print("Response:")
    response = stream_llm_response(prompt)
