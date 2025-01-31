# AItocomplete

AItocomplete is a simple Emacs minor mode that integrates Ollama via a REST API for autocompletion. It allows users to leverage any local LLM model they choose, making it a flexible and customizable solution.

## Features

* Uses a local LLM server for code and text autocompletion
* Works with any model available on Ollama
* Lightweight and minimal setup required

## Installation

Ensure you have Ollama installed and running locally.

Clone this repository and place llm-autocomplete.el in your Emacs load-path.

Add the following to your Emacs config:

```elisp
    (require 'llm-autocomplete)
    (llm-autocomplete-mode 1)
```

## Usage

The mode will automatically send context before the cursor to the LLM and suggest completions.

Configure the model and API endpoint with:

```elisp
(setq llm-autocomplete-endpoint "http://localhost:11434/api/generate")
(setq llm-autocomplete-model "your-model-name")
```

## Notes

This project is experimental and may require tweaks depending on your model and workflow. Contributions and improvements are welcome!

