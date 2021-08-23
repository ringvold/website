module.exports = {
  variants: [],
  theme: {
    extend: {
      backgroundImage: theme => ({
        'frontpage': "url('bg.jpg')",
      }),
      colors: {
        frontpageH2: "var(--frontpage-h2)",
      }
    }
  },
  plugins: [
    require("@tailwindcss/typography"),
  ],
};