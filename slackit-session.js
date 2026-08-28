(() => {
  const boot = globalThis.TS && globalThis.TS.boot_data;
  if (!boot || typeof boot !== "object") return null;

  const text = (value) => {
    if (value == null) return null;
    const result = String(value).trim();
    return result || null;
  };

  const token = text(boot.api_token);
  if (!token || !token.startsWith("xoxc-")) return null;

  const team = boot.team && typeof boot.team === "object" ? boot.team : null;
  const user = boot.user && typeof boot.user === "object" ? boot.user : null;
  const self = boot.self && typeof boot.self === "object" ? boot.self : null;

  return {
    token,
    teamId: text(boot.team_id) || text(team && team.id),
    userId:
      text(boot.user_id) ||
      text(user && user.id) ||
      text(self && self.id)
  };
})()
