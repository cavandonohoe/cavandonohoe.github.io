import { createClient as createAdminClient } from "@supabase/supabase-js";
import type { Database } from "../database.types";

// Service-role client for privileged, server-only operations (e.g. minting a
// session for the single ranker account after a local password check). Never
// import this from client components — the service role key must stay on the
// server.
export function createServiceClient() {
  return createAdminClient<Database>(
    process.env.NEXT_PUBLIC_SUPABASE_URL!,
    process.env.SUPABASE_SERVICE_ROLE_KEY!,
    {
      auth: {
        autoRefreshToken: false,
        persistSession: false
      }
    }
  );
}
