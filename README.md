<p align="center">
  <img style='height: auto; width: 40%; object-fit: contain' src="./aidermacs.png">
</p>

# Aidermacs: AI Pair Programming in Emacs

## Introduction

Miss using [Cursor](https://cursor.sh) but prefer working in Emacs? Aidermacs brings Cursor-like AI-powered development to your Emacs workflow by integrating [Aider](https://github.com/paul-gauthier/aider), one of the most powerful open-source AI pair programming tools available. As a community-driven project, Aidermacs prioritizes the needs and preferences of Emacs users. It provides the same powerful features you'd find in Cursor:

- Top performance on the SWE Bench, solving real GitHub issues in major open source projects
- Seamless Git integration with automatic, sensible commit messages
- Support for multi-file edits in complex codebases
- Real-time file synchronization for true pair programming
- Broad language support including Python, JavaScript, TypeScript, PHP, HTML, CSS, and more
- Compatibility with leading AI models like Claude 3.5 Sonnet, DeepSeek, and GPT-4o

### Community-Driven Development

Aidermacs thrives on community involvement. We believe that the best software is built collaboratively, with input from users and contributors. We encourage you to:

- Contribute Code: Submit pull requests with bug fixes, new features, or improvements to existing functionality.
- Report Issues: Let us know about any bugs, unexpected behavior, or feature requests through GitHub Issues.
- Share Ideas: Participate in discussions and propose new ideas for making Aidermacs even better.
- Improve Documentation: Help us make the documentation clearer, more comprehensive, and easier to use.

Your contributions are essential to making Aidermacs the best AI pair programming tool for Emacs!

### Why Aidermacs over aider.el?

Aidermacs is designed to provide a more Emacs-native experience while still integrating with [Aider](https://github.com/paul-gauthier/aider). It began as a fork of [aider.el](https://github.com/tninja/aider.el), but has since diverged significantly to prioritize Emacs workflow integration.

While `aider.el` strictly mirrors Aider's CLI behavior, `Aidermacs` is built around Emacs-specific features and paradigms. This design philosophy allows you to harness Aider's powerful capabilities through a natural, Emacs-native coding experience.

With `Aidermacs`, you get:

1. Intelligent Model Selection
   - Automatic discovery of available models from multiple providers
   - Real-time model compatibility checking
   - Seamless integration with your configured API keys
   - Caching for quick access to frequently used models
   - Support for both popular pre-configured models and dynamically discovered ones

2. Flexible Terminal Backend Support
   - `Aidermacs` supports multiple terminal backends (comint and vterm) for better compatibility and performance
   - Easy configuration to choose your preferred terminal emulation
   - Extensible architecture for adding new backends

3. Smarter Syntax Highlighting
   - AI-generated code appears with proper syntax highlighting in major languages.
   - Ensures clarity and readability without additional configuration.

4. Better Support for Multiline Input
   - `aider` is primarily designed as a command-line program, where multiline input is restricted by terminal limitations.
   - Terminal-based tools require special syntax or manual formatting to handle multiline input, which can be cumbersome and unintuitive.
   - `Aidermacs` eliminates these restrictions by handling multiline prompts natively within Emacs, allowing you to compose complex AI requests just like any other text input.
   - Whether you're pasting blocks of code or refining AI-generated responses, multiline interactions in `Aidermacs` feel natural and seamless.

5. Enhanced File Management from Emacs
   - List files currently in chat with `M-x aidermacs-list-added-files`
   - Drop specific files from chat with `M-x aidermacs-drop-file`
   - View output history with `M-x aidermacs-show-output-history`
   - Interactively select files to add with `M-x aidermacs-add-files-interactively`
   - and more

6. Greater Configurability
   - `Aidermacs` offers more customization options to tailor the experience to your preferences.

7. Streamlined Transient Menu Selection
   - The transient menus have been completely redesigned to encompass functionality and ergonomics, prioritizing user experience.

8. Community-Driven Development
   - `Aidermacs` is actively developed and maintained by the community, incorporating user feedback and contributions.
   - We prioritize features and improvements that directly benefit Emacs users, ensuring a tool that evolves with your needs.

... and more to come 🚀

## Installation

### Requirements
- Emacs ≥ 26.1
- [Aider](https://aider.chat/docs/install.html)
- [Transient](https://github.com/magit/transient)

### Sample Config With Straight
```emacs-lisp
(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-default-model "anthropic/claude-3-5-sonnet-20241022")
  (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu))
```

### Default Model Selection

You can customize the default AI model used by Aidermacs by setting the `aidermacs-default-model` variable:

```emacs-lisp
(setq aidermacs-default-model "anthropic/claude-3-5-sonnet-20241022")
```

This allows you to easily switch between different AI models without modifying the `aidermacs-extra-args` variable.

### Customizing Aider Options with `aidermacs-extra-args`

The `aidermacs-extra-args` variable allows you to pass any command-line options supported by Aider. See the [Aider configuration documentation](https://aider.chat/docs/config/options.html) for a full list of available options.

For example, to set the verbosity:

```emacs-lisp
(setq aidermacs-extra-args '("--verbose"))
```

These arguments will be appended to the Aider command when it is run. Note that the `--model` argument is automatically handled by `aidermacs-default-model` and should not be included in `aidermacs-extra-args`.

### Sample Config With Doom Emacs
```emacs-lisp
(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))
```

## Configuration

### Terminal Backend Selection

Choose your preferred terminal backend by setting `Aidermacs-backend`:

`vterm` provides better terminal compatibility and bracketed paste support, while `comint` is a simpler, built-in option.

```emacs-lisp
;; Use vterm backend (default is comint)
(setq aidermacs-backend 'vterm)
```

Available backends:
- `comint` (default): Uses Emacs' built-in terminal emulation
- `vterm`: Leverages vterm for better terminal compatibility

### Re-Enable Auto-Commits

Aider by default automatically commits changes made by the AI. We find this behavior /very/ intrusive, so we disabled it for you. You can re-enable auto-commits by setting `aidermacs-auto-commits` to `t`:

```emacs-lisp
;; Enable auto-commits
(setq aidermacs-auto-commits t)
```

With auto-commits disabled, you'll need to manually commit changes using your preferred Git workflow.

### Multiline Input Configuration

When using the comint backend, you can customize the key binding for multiline input:

```emacs-lisp
;; Change multiline input key (default is S-<return>)
(setq aidermacs-comint-multiline-newline-key "C-<return>")
```

This key allows you to enter multiple lines without sending the command to Aider. Press `RET` normally to send the command.

## Usage

This section provides a step-by-step guide on how to use Aidermacs for AI-assisted pair programming in Emacs.

### Getting Started

The main interface to Aidermacs is through its transient menu system (similar to Magit). Access it with:

```
M-x aidermacs-transient-menu
```

Or bind it to a key in your config:

```emacs-lisp
(global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
```

### Core Workflow

#### 1. Start a Session
From the transient menu:
- `a` Start/open session (auto-detects project root)
- `.` Start in current directory (good for monorepos)
- `^` Toggle "Start in New Frame" option
- `o` Change AI model
- `s` Reset session
- `x` Exit session

The session buffer will be named `*aidermacs:your-repo-name*` where you can interact with the AI.

#### 2. Quick Actions
Most common operations are available directly:
- `f` Add current file to chat
- `c` Request code changes
- `r` Refactor code at point/region
- `d` Drop current file from chat
- `g` Accept AI's proposal ("go ahead")
- `u` Undo last change
- `Q` Ask general question
- `q` Ask question about current code
- `e` Explain code at point/region
- `p` Explain symbol under point

#### 3. Specialized Commands
Access more specific commands through submenus:

##### File Commands (`F`)
- Add files: current (`f`), read-only (`r`), window (`w`), directory (`d`), marked in dired (`m`)
- Drop files: specific (`j`), current (`k`)
- List files in chat (`l`)

##### Code Commands (`C`)
- Code changes (`c`)
- Refactoring (`r`)
- Implement TODOs (`i`)
- Testing: write (`t`), fix (`T`)
- Debug exceptions (`x`)
- Undo changes (`u`)

##### Understanding Code
- `m` Show last commit
- `q` Ask questions
- `e` Explain code at point/region
- `p` Explain symbol under point

##### Other Features
- `H` View session history
- `L` Copy last AI output
- `l` Clear buffer
- `h` Get help

### Working with Code Blocks

When editing `.aider.prompt.org` or other files, these keybindings are available:

- `C-c C-n` or `C-<return>`: Send line/region line-by-line
- `C-c C-c`: Send block/region as whole
- `C-c C-z`: Switch to Aidermacs buffer

### Prompt Files

The `.aider.prompt.org` file (created with `M-x aidermacs-open-prompt-file`) is useful for:
- Storing frequently used prompts
- Documenting common workflows
- Quick access to complex instructions

The file is automatically recognized and enables Aidermacs minor mode with the above keybindings.

### Dynamic Model Selection

Aidermacs provides intelligent model selection that automatically detects and integrates with multiple AI providers:

- Automatically fetches available models from supported providers (OpenAI, Anthropic, DeepSeek, Google Gemini, OpenRouter)
- Caches model lists for quick access
- Supports both popular pre-configured models and dynamically discovered ones
- Handles API keys and authentication automatically
- Provides model compatibility checking

To change models:
1. Use `M-x aidermacs-change-model` or press `o` in the transient menu
2. Select from either:
   - Popular pre-configured models (fast)
   - Dynamically fetched models from all supported providers (comprehensive)

The system will automatically filter models to only show ones that are:
- Supported by your current Aider version
- Available through your configured API keys
- Compatible with your current workflow
