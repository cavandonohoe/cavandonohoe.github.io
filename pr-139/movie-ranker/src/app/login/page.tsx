"use client";

import { Suspense, useMemo, useState } from "react";
import { useSearchParams } from "next/navigation";
import { createClient } from "@/lib/supabase/client";

function LoginInner() {
  const supabase = useMemo(() => createClient(), []);
  const searchParams = useSearchParams();
  const next = searchParams.get("next") ?? "/compare";

  const [email, setEmail] = useState("");
  const [status, setStatus] = useState<"idle" | "sending" | "sent" | "error">(
    "idle"
  );
  const [error, setError] = useState<string | null>(null);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!email.trim()) return;
    setStatus("sending");
    setError(null);

    const redirectTo = `${window.location.origin}/auth/callback?next=${encodeURIComponent(
      next
    )}`;
    const { error: signInError } = await supabase.auth.signInWithOtp({
      email: email.trim(),
      options: { emailRedirectTo: redirectTo }
    });

    if (signInError) {
      setStatus("error");
      setError(signInError.message);
      return;
    }
    setStatus("sent");
  };

  return (
    <section className="mx-auto flex min-h-[70vh] max-w-md flex-col justify-center">
      <h1 className="font-display text-4xl font-semibold tracking-tight sm:text-5xl">
        Sign in
      </h1>
      <p className="mt-2 text-mist">
        Magic link, no password. Your rankings stay yours.
      </p>

      {status === "sent" ? (
        <div className="mt-8 rounded-2xl border border-white/5 bg-slate p-6">
          <h2 className="font-display text-2xl font-semibold">
            Check your inbox
          </h2>
          <p className="mt-2 text-sm text-mist">
            We just sent a magic link to{" "}
            <span className="font-medium text-ink">{email}</span>. Click it on
            this device to sign in.
          </p>
          <button
            type="button"
            onClick={() => setStatus("idle")}
            className="mt-4 text-xs uppercase tracking-[0.18em] text-gold hover:underline"
          >
            Use a different email
          </button>
        </div>
      ) : (
        <form onSubmit={handleSubmit} className="mt-8 space-y-4">
          <label className="block">
            <span className="mb-1.5 block text-xs font-medium uppercase tracking-[0.18em] text-mist">
              Email
            </span>
            <input
              type="email"
              required
              autoFocus
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              placeholder="you@example.com"
              className="w-full rounded-xl border border-white/10 bg-slate px-4 py-3 text-base text-ink placeholder:text-mist/50 focus:border-white/30 focus:outline-none focus:ring-2 focus:ring-gold/30"
            />
          </label>

          {error && (
            <p className="rounded-xl bg-ember/10 p-3 text-sm text-ember">
              {error}
            </p>
          )}

          <button
            type="submit"
            disabled={status === "sending"}
            className="inline-flex w-full items-center justify-center rounded-full bg-gold px-7 py-4 text-base font-semibold text-cream shadow-card transition hover:bg-ember hover:text-ink disabled:cursor-not-allowed disabled:opacity-60"
          >
            {status === "sending" ? "Sending…" : "Send magic link"}
          </button>
        </form>
      )}
    </section>
  );
}

export default function LoginPage() {
  return (
    <Suspense fallback={null}>
      <LoginInner />
    </Suspense>
  );
}
