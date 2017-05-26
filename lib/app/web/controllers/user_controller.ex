defmodule App.Web.UserController do
  use App.Web, :controller

  action_fallback App.Web.FallbackController

  def login(conn, %{"username" => username, "password" => password}) do
    IO.puts("Logging in: #{username} #{password}")
    user = App.Colleagues.login(username, password)
    json conn, %{email: user.email, location: user.location, name: user.name, timezone: user.timezone}
  end

  def signup(conn, %{"email" => email, "location" => location, "name" => name, "timezone" => timezone}) do
    {:ok, user} = App.Colleagues.create_user(%{email: email, location: location, name: name, timezone: timezone})
    IO.inspect(user)
    App.Web.Endpoint.broadcast("user:all", "new:colleague", %{:colleagues => [App.Colleagues.to_json(user)]})
    json conn, %{email: user.email, location: user.location, name: user.name, timezone: user.timezone}
  end
end
