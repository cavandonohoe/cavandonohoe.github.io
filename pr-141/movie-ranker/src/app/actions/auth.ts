"use server";

import { revalidatePath } from "next/cache";
import { redirect } from "next/navigation";
import { createClient } from "@/lib/supabase/server";
import { createServiceClient } from "@/lib/supabase/admin";

export async function signOutAction() {
  const supabase = createClient();
  await supabase.auth.signOut();
  revalidatePath("/", "layout");
  redirect("/");
}

// Local, single-user password login. The password lives only in the
// gitignored `.env.local` (`RANKER_PASS`), never in the committed source, so
// the public repo leaks nothing. On a match we mint a real Supabase session
// for the fixed ranker account (`RANKER_EMAIL`) server-side via the service
// role — no email is sent, so there is no rate limit — and the SSR client
// writes the session cookies. This keeps `auth.uid()` working for RLS.
export async function passwordLoginAction(formData: FormData) {
  const password = String(formData.get("password") ?? "");
  const next = String(formData.get("next") ?? "/compare");

  const expected = process.env.RANKER_PASS;
  const email = process.env.RANKER_EMAIL;
  if (!expected || !email) {
    redirect(`/login?error=${encodeURIComponent("Server is missing RANKER_PASS / RANKER_EMAIL")}`);
  }
  if (password !== expected) {
    redirect(`/login?error=${encodeURIComponent("Wrong password")}`);
  }

  const admin = createServiceClient();
  const { data, error: linkError } = await admin.auth.admin.generateLink({
    type: "magiclink",
    email: email!
  });
  if (linkError || !data?.properties?.hashed_token) {
    redirect(
      `/login?error=${encodeURIComponent(linkError?.message ?? "Could not create session")}`
    );
  }

  const supabase = createClient();
  const { error: verifyError } = await supabase.auth.verifyOtp({
    type: "magiclink",
    token_hash: data!.properties.hashed_token
  });
  if (verifyError) {
    redirect(`/login?error=${encodeURIComponent(verifyError.message)}`);
  }

  revalidatePath("/", "layout");
  redirect(next);
}
