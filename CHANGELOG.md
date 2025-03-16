# CHANGELOG

## 1.0 - Initial Release

### Features

-   **Built-in Ediff Integration:** Review AI-generated changes with Emacs' familiar `ediff` interface.
-   **Intelligent Model Selection:** Automatically discover and integrate with multiple AI providers (OpenAI, Anthropic, DeepSeek, Google Gemini, OpenRouter).
-   **Flexible Terminal Backend Support:** Choose between `comint` and `vterm` backends for the best terminal compatibility and performance.
-   **Enhanced File Management:** Easily manage files within your Aider session with commands for adding, dropping, listing, and more. Full support for remote files via Tramp (SSH, Docker, etc.).
-   **Streamlined Transient Menu Selection:** Access all Aidermacs features through a redesigned and ergonomic transient menu system.
-   **Prompt Files Minor Mode:** Work seamlessly with prompt files and other Aider-related files with convenient keybindings and automatic mode activation.
-   **Claude 3.7 Sonnet Thinking Tokens:** Enable and configure thinking tokens using the `/think-tokens` in-chat command or the `--thinking-tokens` command-line argument.
-   **Architect Mode Confirmation:** Control whether to automatically accept Architect mode changes with the `aidermacs-auto-accept-architect` variable.
-   **Re-Enable Auto-Commits:** Aider automatically commits AI-generated changes by default. We consider this behavior *very* intrusive, so we've disabled it. You can re-enable auto-commits by setting `aidermacs-auto-commits` to `t`.
-   **Customizing Aider Options with `aidermacs-extra-args`:** Pass any Aider-supported command-line options.
