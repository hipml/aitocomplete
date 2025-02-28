# AItoComplete

AItoComplete is an Emacs package that provides a convenient interface to Ollama, allowing you to access LLM capabilities directly from your Emacs workflow.

## Prerequisites

- Emacs 26.1 or higher
- [Ollama](https://ollama.ai/) installed and running locally
- curl command-line tool

## Installation

### Manual Installation

1. Download `aitocomplete.el` to a directory in your Emacs load path.
2. Add the following to your `init.el`:

```elisp
(require 'aitocomplete)
```

### Using straight.el and use-package

```elisp
(use-package aitocomplete
  :straight (:host github :repo "yourusername/aitocomplete")
  :bind (("C-c s" . aitocomplete-send-region)
         ("C-c a" . aitocomplete-menu)))
```

### Using MELPA (once it's available)

```elisp
(use-package aitocomplete
  :ensure t)
```

## Configuration

The package works out of the box, but you can customize it to your preferences:

```elisp
;; Set your preferred model
(setq aitocomplete-model "llama3.2")  ; Default model

;; Customize buffer name
(setq aitocomplete-chat-buffer "*My AI Assistant*")

;; Set the number of columns in the menu
(setq aitocomplete-menu-columns 4)

;; Custom keybinding for the menu
(global-set-key (kbd "C-c a") #'aitocomplete-menu)
```

## Usage

### Quick Start

1. Start the Ollama server on your local machine
2. Select a region of text in any buffer
3. Press `C-c s` to send the region to the AI
4. View the AI's response in the chat buffer

### Main Commands

- `M-x aitocomplete-menu`: Display the main menu with all options
- `M-x aitocomplete-send-region`: Send the selected region to the AI
- `M-x aitocomplete-chat`: Open or switch to the AI chat buffer
- `M-x aitocomplete-test-response`: Test the AI connection with a simple prompt

### In the Chat Buffer

- Type your message
- Press `C-c C-c` to send it to the AI
- Press `?` to display the menu

## Default Keybindings

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c s` | `aitocomplete-send-region` | Send selected text to AI |
| `C-c C-c` | `aitocomplete-send-message` | (In chat buffer) Send message |
| `?` | `aitocomplete-menu` | (In chat buffer) Display menu |

## Menu Options

Press `M-x aitocomplete-menu` or `?` in the chat buffer to access these options:

- `[o]` Open chat buffer
- `[s]` Send region
- `[m]` Change model
- `[t]` Test response
- `[q]` Quit menu

## Supported Models

By default, the package supports whatever models you have available in your local Ollama installation. You can check available models by opening the menu.

## Troubleshooting

### Ollama Server Not Running

If you see "NOT RUNNING" in the menu, make sure:
1. Ollama is installed on your system
2. The Ollama server is running
3. It's accessible at http://localhost:11434

### Debug Messages

Enable debug messages to help troubleshoot:

```elisp
(setq debug-on-error t)
```

## Contributing

Contributions are welcome! Feel free to open issues or submit pull requests on GitHub.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

AItoComplete was inspired by similar projects that integrate AI capabilities into Emacs.
