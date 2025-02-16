<p align="center">
  <img style='height: auto; width: 40%; object-fit: contain' src="./aidermacs.png">
</p>

# Aidermacs: AI Pair Programming in Emacs

Miss using [Cursor](https://cursor.sh) but prefer living in Emacs? Aidermacs brings Cursor-like AI-powered development to your Emacs workflow by integrating [Aider](https://github.com/paul-gauthier/aider), one of the most powerful open-source AI pair programming tools available. As a community-driven project, Aidermacs prioritizes the needs and preferences of Emacs users. It provides the same powerful features you'd find in Cursor:

Key features:

- Top performance on the SWE Bench, solving real GitHub issues in major open source projects
- Support for multi-file edits in complex codebases
- Real-time file synchronization for true pair programming
- Broad language support including Python, JavaScript, TypeScript, PHP, HTML, CSS, etc.
- Compatibility with leading AI models like Claude 3.5 Sonnet, DeepSeek, and GPT-4o, etc.

## Why Aidermacs?

Aidermacs delivers an Emacs-centric experience by deeply integrating with Emacs paradigms, made by Emacs users, for Emacs users. This includes ability to separate reasoning and code editing model, intelligent model selection, flexible terminal backend support (comint and vterm), smarter syntax highlighting, enhanced file management, streamlined transient menus, and more.

### Community-Driven Development

Aidermacs thrives on community involvement. We believe that the best software is built collaboratively, with input from users and contributors. We encourage you to:

- Contribute Code: Submit pull requests with bug fixes, new features, or improvements to existing functionality.
- Report Issues: Let us know about any bugs, unexpected behavior, or feature requests through GitHub Issues.
- Share Ideas: Participate in discussions and propose new ideas for making Aidermacs even better.
- Improve Documentation: Help us make the documentation clearer, more comprehensive, and easier to use.

Your contributions are essential to making Aidermacs the best AI pair programming tool in Emacs!

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
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  ; See the Configuration section below
  (setq aidermacs-auto-commits t)
  (setq aidermacs-use-architect-mode t))
```

### Sample Config With Doom Emacs
```emacs-lisp
(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))
```

## Configuration

### Default Model Selection

You can customize the default AI model used by Aidermacs by setting the `aidermacs-default-model` variable:

```emacs-lisp
(setq aidermacs-default-model "anthropic/claude-3-5-sonnet-20241022")
```

This allows you to easily switch between different AI models without modifying the `aidermacs-extra-args` variable.

*Note: This configuration will be overwritten by the existence of an `.aider.conf.yml` file (see [details](#Overwrite-Configuration-with-Configuration-File)).*

### Dynamic Model Selection

Aidermacs provides intelligent model selection for the solo (non-Architect) mode that automatically detects and integrates with multiple AI providers:

- Automatically fetches available models from supported providers (OpenAI, Anthropic, DeepSeek, Google Gemini, OpenRouter)
- Caches model lists for quick access
- Supports both popular pre-configured models and dynamically discovered ones
- Handles API keys and authentication automatically
- Provides model compatibility checking

The dynamic model selection is only for the solo (non-Architect) mode.

To change models in solo mode:
1. Use `M-x aidermacs-change-model` or press `o` in the transient menu
2. Select from either:
   - Popular pre-configured models (fast)
   - Dynamically fetched models from all supported providers (comprehensive)

The system will automatically filter models to only show ones that are:
- Supported by your current Aider version
- Available through your configured API keys
- Compatible with your current workflow

### Architect Mode - Separating Code Reasoning and Editing Models

Aidermacs supports an experimental mode that leverages two models for each coding task: an Architect model for reasoning and an Editor model for generating code edits. This approach has **achieved state-of-the-art (SOTA) results on aider's code editing benchmark**, as detailed in [this blog post](https://aider.chat/2024/09/26/architect.html).

To enable this mode, set `aidermacs-use-architect-mode` to `t`. You must also configure the `aidermacs-architect-model` variable to specify the model to use for the Architect role.

By default, the `aidermacs-editor-model` is the same as `aidermacs-default-model`. You only need to set `aidermacs-editor-model` if you want to use a different model for the Editor role.

When Architect mode is enabled, the `aidermacs-default-model` setting is ignored, and `aidermacs-architect-model` and `aidermacs-editor-model` are used instead.

```emacs-lisp
(setq aidermacs-use-architect-mode t)
(setq aidermacs-architect-model "o1-mini") ; default
(setq aidermacs-editor-model "deepseek/deepseek-chat") ;; defaults to aidermacs-default-model
```

*Note: This configuration will be overwritten by the existence of an `.aider.conf.yml` file (see [details](#Overwrite-Configuration-with-Configuration-File)).*

### Terminal Backend Selection

Choose your preferred terminal backend by setting `aidermacs-backend`:

`vterm` provides better terminal compatibility, while `comint` is a simple, built-in option that's still fully compatible with aidermacs.

```emacs-lisp
;; Use vterm backend (default is comint)
(setq aidermacs-backend 'vterm)
```

Available backends:
- `comint` (default): Uses Emacs' built-in terminal emulation
- `vterm`: Leverages vterm for better terminal compatibility


### Multiline Input Configuration

When using the comint backend, you can customize the key binding for multiline input:

```emacs-lisp
;; Change multiline input key (default is S-<return>)
(setq aidermacs-comint-multiline-newline-key "C-<return>")
```

This key allows you to enter multiple lines without sending the command to Aider. Press `RET` normally to send the command.

### Re-Enable Auto-Commits

Aider by default automatically commits changes made by the AI. We find this behavior *very* intrusive, so we disabled it for you. You can re-enable auto-commits by setting `aidermacs-auto-commits` to `t`:

```emacs-lisp
;; Enable auto-commits
(setq aidermacs-auto-commits t)
```

With auto-commits disabled, you'll need to manually commit changes using your preferred Git workflow.

*Note: This configuration will be overwritten by the existence of an `.aider.conf.yml` file (see [details](#Overwrite-Configuration-with-Configuration-File)).*

### Customizing Aider Options with `aidermacs-extra-args`

If the above configurations aren't enough already, the `aidermacs-extra-args` variable allows you to pass any command-line options supported by Aider.

See the [Aider configuration documentation](https://aider.chat/docs/config/options.html) for a full list of available options.

```emacs-lisp
;; Set the verbosity:
(setq aidermacs-extra-args '("--verbose"))
```

These arguments will be appended to the Aider command when it is run. Note that the `--model` argument is automatically handled by `aidermacs-default-model` and should not be included in `aidermacs-extra-args`.


### Overwrite Configuration with Configuration File

Aidermacs supports project-specific configurations via `.aider.conf.yml` files. To enable this:

1.  Create a `.aider.conf.yml` in your home dir, project's root, or the current directory, defining your desired settings. See the [Aider documentation](https://aider.chat/docs/config/aider_conf.html) for available options.

2.  Tell Aidermacs to use the config file in one of two ways:

    ```emacs-lisp
    ;; Set the `aidermacs-config-file` variable in your Emacs config:
    (setq aidermacs-config-file "/path/to/your/project/.aider.conf.yml")
    ;; *Or*, include the `--config` or `-c` flag in `aidermacs-extra-args`:
    (setq aidermacs-extra-args '("--config" "/path/to/your/project/.aider.conf.yml"))
    ```

*Note: You can also rely on Aider's default behavior of automatically searching for `.aider.conf.yml` in the home directory, project root, or current directory, in that order. In this case, you do not need to set `aidermacs-config-file` or include `--config` in `aidermacs-extra-args`.*

**Important: When using a config file, all other Aidermacs configuration variables supplying an argument option (e.g., `aidermacs-default-model`, `aidermacs-architect-model`, `aidermacs-use-architect-mode`) are IGNORED.** Aider will *only* use the settings specified in your `.aider.conf.yml` file. Do not attempt to combine these Emacs settings with a config file, as the results will be unpredictable.

#### Notes

*   **Precedence:** Settings in `.aider.conf.yml` *always* take precedence when a config file is explicitly specified.
*   **Avoid Conflicts:** When using a config file, *do not* include model-related arguments (like `--model`, `--architect`, etc.) in `aidermacs-extra-args`.  Configure *all* settings within your `.aider.conf.yml` file.

## Usage

### Getting Started

The main interface to Aidermacs is through its transient menu system (similar to Magit). Access it with:

```
M-x aidermacs-transient-menu
```

Or bind it to a key in your config:

```emacs-lisp
(global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
```

The main interface to Aidermacs is through its transient menu system. Here's a breakdown of the available commands:

##### Core Actions
- `a`: Start/Open Session (auto-detects project root)
- `.` : Start in Current Directory (good for monorepos)
- `o`: Change Solo Model
- `s`: Reset Session
- `x`: Exit Session

##### Quick Actions
- `f`: Add Current File
- `c`: Code Change
- `r`: Refactor
- `g`: Go Ahead

##### File Commands (`F`)
- Add Files: 
  - Current file (`f`, C-u for read-only)
  - Interactively (`i`)
  - Window files (`w`, C-u for read-only)
  - Directory files (`d`, C-u for read-only)
  - Dired marked files (`m`, C-u for read-only)
- Drop Files: interactively (`j`), current (`k`), all (`a`)
- Other Actions: list files (`l`)

##### Code Commands (`C`)
- Code Actions: code change (`c`), refactor code (`r`), implement TODO (`i`), write tests (`t`), fix test (`T`), debug exception (`x`), undo changes (`u`)

##### Understanding
- `m`: Show Last Commit
- `Q`: Ask General Question
- `q`: Ask Question
- `e`: Explain This Code
- `p`: Explain This Symbol

##### Others
- `A`: Toggle Architect Mode (Separate Reasoner/Editor)
- `H`: Session History
- `L`: Copy Last Aidermacs Output
- `O`: Clear Model Selection Cache
- `l`: Clear Buffer
- `h`: Aider Help

### Working with Prompt Blocks in `.aider*` files

When editing `.aider.prompt.org` or other `.aider*` files, these keybindings are available:

- `C-c C-n` or `C-<return>`: Send line/region line-by-line
- `C-c C-c`: Send block/region as whole
- `C-c C-z`: Switch to Aidermacs buffer

### Prompt Files

The `.aider.prompt.org` file (created with `M-x aidermacs-open-prompt-file`) is useful for:
- Storing frequently used prompts
- Documenting common workflows
- Quick access to complex instructions

The file is automatically recognized and enables Aidermacs minor mode with the above keybindings.

## Aidermacs vs aider.el

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
