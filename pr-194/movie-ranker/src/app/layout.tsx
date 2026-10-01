import type { Metadata } from "next";
import { Inter, Fraunces } from "next/font/google";
import Link from "next/link";
import { createClient } from "@/lib/supabase/server";
import { signOutAction } from "@/app/actions/auth";
import "./globals.css";

const inter = Inter({
  subsets: ["latin"],
  variable: "--font-inter",
  display: "swap"
});

const fraunces = Fraunces({
  subsets: ["latin"],
  variable: "--font-fraunces",
  display: "swap"
});

export const metadata: Metadata = {
  title: "Reel — rank your movies and series",
  description:
    "A personal pairwise ranker for everything I've ever rated on IMDb. Resolve the ties one tap at a time."
};

export default async function RootLayout({
  children
}: {
  children: React.ReactNode;
}) {
  const supabase = createClient();
  const {
    data: { user }
  } = await supabase.auth.getUser();

  return (
    <html lang="en" className={`${inter.variable} ${fraunces.variable}`}>
      <body className="min-h-screen bg-cream text-ink antialiased">
        <header className="sticky top-0 z-30 border-b border-white/5 bg-cream/80 backdrop-blur">
          <nav className="mx-auto flex max-w-6xl items-center justify-between px-5 py-4">
            <Link
              href="/"
              className="font-display text-xl font-semibold tracking-tight"
            >
              Reel
            </Link>
            <div className="flex items-center gap-1 text-sm sm:gap-2">
              <NavLink href="/compare">Compare</NavLink>
              <NavLink href="/leaderboard">Leaderboard</NavLink>
              {user ? (
                <form action={signOutAction}>
                  <button
                    type="submit"
                    className="rounded-full px-3 py-1.5 text-mist transition hover:bg-white/5 hover:text-ink"
                    title={user.email ?? undefined}
                  >
                    Sign out
                  </button>
                </form>
              ) : (
                <NavLink href="/login">Sign in</NavLink>
              )}
            </div>
          </nav>
        </header>
        <main className="mx-auto max-w-6xl px-5 pb-16 pt-6">{children}</main>
        <footer className="border-t border-white/5 py-8 text-center text-xs text-mist">
          Built with Next.js, Tailwind, and Supabase.
        </footer>
      </body>
    </html>
  );
}

function NavLink({
  href,
  children
}: {
  href: string;
  children: React.ReactNode;
}) {
  return (
    <Link
      href={href}
      className="rounded-full px-3 py-1.5 text-ink/80 transition hover:bg-white/5 hover:text-ink"
    >
      {children}
    </Link>
  );
}
