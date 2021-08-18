const shiki = require("shiki");

module.exports = {
    highlight: async function (fromElm) {
        const highlighter = shiki.getHighlighter({
            theme: "dark-plus",
        });
        return await highlighter.then((highlighter) => {
            return {
                tokens: highlighter.codeToThemedTokens(
                    fromElm.body,
                    fromElm.language,
                    highlighter.getTheme(),
                    {
                        includeExplanation: false,
                    }
                ),
                bg: highlighter.getTheme().bg,
                fg: highlighter.getTheme().fg,
            };
        });
    },
};