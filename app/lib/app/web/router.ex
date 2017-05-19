defmodule App.Web.Router do
  use App.Web, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", App.Web do
    pipe_through :api
  end
end
