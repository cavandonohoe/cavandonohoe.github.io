"use client";

import type { Title } from "@/lib/database.types";

interface TitleCardProps {
  title: Title;
  onSelect?: () => void;
  disabled?: boolean;
  highlight?: "winner" | "loser" | null;
}

export function TitleCard({
  title,
  onSelect,
  disabled,
  highlight
}: TitleCardProps) {
  const stars = "★".repeat(title.your_rating) + "☆".repeat(10 - title.your_rating);
  return (
    <button
      type="button"
      onClick={onSelect}
      disabled={disabled}
      className={[
        "group relative flex w-full flex-col overflow-hidden rounded-3xl bg-slate text-left",
        "border border-white/5 shadow-card transition-all",
        "disabled:cursor-not-allowed",
        highlight === "winner"
          ? "ring-2 ring-gold scale-[1.01]"
          : highlight === "loser"
            ? "opacity-40"
            : "hover:-translate-y-1 hover:border-white/20 hover:shadow-[0_24px_60px_-20px_rgba(0,0,0,0.7)] active:scale-[0.99]"
      ].join(" ")}
    >
      <div className="relative flex aspect-[3/4] w-full items-end overflow-hidden bg-gradient-to-br from-cream to-slate p-5 sm:p-7">
        {title.image_url ? (
          // eslint-disable-next-line @next/next/no-img-element
          <img
            src={title.image_url}
            alt={title.title}
            className="absolute inset-0 h-full w-full object-cover transition-transform duration-500 ease-out group-hover:scale-105"
          />
        ) : (
          <div className="pointer-events-none absolute inset-0 flex items-center justify-center text-[12rem] font-display font-bold leading-none text-white/[0.03] sm:text-[18rem]">
            {title.year ?? ""}
          </div>
        )}
        <span className="absolute left-4 top-4 rounded-full bg-cream/80 px-3 py-1 text-[11px] font-medium uppercase tracking-wider text-ink/80 backdrop-blur">
          {title.title_type ?? title.kind}
        </span>
        <span className="absolute right-4 top-4 rounded-full bg-gold/90 px-3 py-1 text-[11px] font-semibold tracking-wider text-cream backdrop-blur">
          {title.your_rating}/10
        </span>
        <div className="relative z-10">
          <div className="text-[11px] uppercase tracking-[0.18em] text-mist">
            {stars}
          </div>
          <h3 className="mt-2 font-display text-3xl font-semibold leading-[1.05] tracking-tight text-ink sm:text-4xl">
            {title.title}
          </h3>
          <div className="mt-2 flex flex-wrap gap-x-3 text-xs text-mist">
            {title.year && <span>{title.year}</span>}
            {title.primary_genre && <span>· {title.primary_genre}</span>}
            {title.directors.length > 0 && (
              <span>· {title.directors.slice(0, 2).join(", ")}</span>
            )}
            {title.imdb_rating != null && (
              <span>· IMDb {title.imdb_rating}</span>
            )}
          </div>
        </div>
      </div>
    </button>
  );
}
