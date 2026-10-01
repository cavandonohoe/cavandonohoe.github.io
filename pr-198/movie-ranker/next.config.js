/** @type {import('next').NextConfig} */
const nextConfig = {
  images: {
    minimumCacheTTL: 2592000,
    remotePatterns: [
      { protocol: "https", hostname: "*.supabase.co" },
      { protocol: "https", hostname: "m.media-amazon.com" },
      { protocol: "https", hostname: "image.tmdb.org" }
    ]
  }
};

module.exports = nextConfig;
