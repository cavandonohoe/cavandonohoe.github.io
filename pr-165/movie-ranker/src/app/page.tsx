import Link from "next/link";

export default function HomePage() {
  return (
    <section className="flex flex-col items-center pt-10 sm:pt-20">
      <span className="rounded-full border border-white/10 bg-white/5 px-3 py-1 text-xs uppercase tracking-[0.18em] text-mist">
        Rank the things you love
      </span>
      <h1 className="mt-6 max-w-3xl text-balance text-center font-display text-5xl font-semibold leading-[1.05] tracking-tight sm:text-7xl">
        Which is{" "}
        <span className="italic text-gold">actually</span> the better one?
      </h1>
      <p className="mt-6 max-w-xl text-balance text-center text-base leading-relaxed text-ink/70 sm:text-lg">
        Two titles, one tap. Or just drag them into the order you want.
        Watch the leaderboard rearrange itself in real time. Movies fight
        movies, series fight series.
      </p>
      <div className="mt-10 flex flex-col items-center gap-3 sm:flex-row">
        <Link
          href="/compare"
          className="group inline-flex items-center justify-center rounded-full bg-gold px-7 py-4 text-base font-semibold text-cream shadow-card transition hover:-translate-y-0.5 hover:bg-ember hover:text-ink"
        >
          Start comparing
          <span aria-hidden className="ml-2 transition-transform group-hover:translate-x-0.5">
            →
          </span>
        </Link>
        <Link
          href="/leaderboard"
          className="inline-flex items-center justify-center rounded-full border border-white/10 bg-white/5 px-7 py-4 text-base font-medium text-ink transition hover:border-white/30"
        >
          See leaderboard
        </Link>
      </div>

      <div className="mt-20 grid w-full max-w-4xl gap-6 sm:grid-cols-3">
        <Feature step="01" title="Pick">
          Two titles. Tap the one you&apos;d watch again first.
        </Feature>
        <Feature step="02" title="Drag">
          Or grab a row on the leaderboard and drop it where it belongs.
        </Feature>
        <Feature step="03" title="Settle">
          Personal vs global rankings, broken out by kind and star bucket.
        </Feature>
      </div>
    </section>
  );
}

function Feature({
  step,
  title,
  children
}: {
  step: string;
  title: string;
  children: React.ReactNode;
}) {
  return (
    <div className="rounded-2xl border border-white/5 bg-slate p-6">
      <div className="text-xs font-medium uppercase tracking-[0.18em] text-gold">
        {step}
      </div>
      <h3 className="mt-2 font-display text-2xl font-semibold">{title}</h3>
      <p className="mt-2 text-sm leading-relaxed text-ink/70">{children}</p>
    </div>
  );
}
