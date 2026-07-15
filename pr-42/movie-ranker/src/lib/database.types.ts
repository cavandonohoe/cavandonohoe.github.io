export type Kind =
  | "movie"
  | "series"
  | "mini_series"
  | "tv_movie"
  | "short"
  | "video_game"
  | "tv_special"
  | "video"
  | "tv_episode"
  | "tv_short"
  | "other";

export type Title = {
  id: string;
  tconst: string;
  title: string;
  original_title: string | null;
  title_type: string | null;
  kind: Kind;
  year: number | null;
  runtime_min: number | null;
  genres: string[];
  primary_genre: string | null;
  directors: string[];
  imdb_url: string | null;
  imdb_rating: number | null;
  num_votes: number | null;
  release_date: string | null;
  date_rated: string | null;
  your_rating: number;
  image_url: string | null;
  rating: number;
  wins: number;
  losses: number;
  created_at: string;
};

export type Comparison = {
  id: string;
  user_id: string;
  winner_title_id: string;
  loser_title_id: string;
  winner_rating_before: number;
  loser_rating_before: number;
  winner_rating_after: number;
  loser_rating_after: number;
  created_at: string;
};

export type ManualOverride = {
  user_id: string;
  title_id: string;
  rating: number;
  created_at: string;
};

export type Database = {
  public: {
    Tables: {
      titles: {
        Row: Title;
        Insert: Omit<
          Title,
          "id" | "rating" | "wins" | "losses" | "created_at"
        > & {
          id?: string;
          rating?: number;
          wins?: number;
          losses?: number;
          created_at?: string;
        };
        Update: Partial<Title>;
      };
      comparisons: {
        Row: Comparison;
        Insert: Omit<Comparison, "id" | "created_at"> & {
          id?: string;
          created_at?: string;
        };
        Update: Partial<Comparison>;
      };
      manual_overrides: {
        Row: ManualOverride;
        Insert: Omit<ManualOverride, "created_at"> & {
          created_at?: string;
        };
        Update: Partial<ManualOverride>;
      };
    };
  };
};
