import { passwordLoginAction } from "@/app/actions/auth";

export default function LoginPage({
  searchParams
}: {
  searchParams: { next?: string; error?: string };
}) {
  const next = searchParams.next ?? "/compare";
  const error = searchParams.error ?? null;

  return (
    <section className="mx-auto flex min-h-[70vh] max-w-md flex-col justify-center">
      <h1 className="font-display text-4xl font-semibold tracking-tight sm:text-5xl">
        Sign in
      </h1>
      <p className="mt-2 text-mist">Enter the password to unlock ranking.</p>

      <form action={passwordLoginAction} className="mt-8 space-y-4">
        <input type="hidden" name="next" value={next} />
        <label className="block">
          <span className="mb-1.5 block text-xs font-medium uppercase tracking-[0.18em] text-mist">
            Password
          </span>
          <input
            type="password"
            name="password"
            required
            autoFocus
            placeholder="••••"
            className="w-full rounded-xl border border-white/10 bg-slate px-4 py-3 text-base text-ink placeholder:text-mist/50 focus:border-white/30 focus:outline-none focus:ring-2 focus:ring-gold/30"
          />
        </label>

        {error && (
          <p className="rounded-xl bg-ember/10 p-3 text-sm text-ember">{error}</p>
        )}

        <button
          type="submit"
          className="inline-flex w-full items-center justify-center rounded-full bg-gold px-7 py-4 text-base font-semibold text-cream shadow-card transition hover:bg-ember hover:text-ink"
        >
          Unlock
        </button>
      </form>
    </section>
  );
}
