import type { Config } from "tailwindcss";

const config: Config = {
  content: ["./src/**/*.{ts,tsx}"],
  theme: {
    extend: {
      fontFamily: {
        sans: ["var(--font-inter)", "system-ui", "sans-serif"],
        display: ["var(--font-fraunces)", "Georgia", "serif"]
      },
      colors: {
        cream: "#0E0E12",
        ink: "#F5F2EA",
        ember: "#E8553D",
        gold: "#F4C95D",
        slate: "#1B1B22",
        mist: "#9A9AA8"
      },
      boxShadow: {
        card: "0 12px 40px -12px rgba(0,0,0,0.55)"
      }
    }
  },
  plugins: []
};

export default config;
