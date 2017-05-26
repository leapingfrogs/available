require Logger

defmodule App.Web.UserChannel do
  use App.Web, :channel

  def join("user:" <> email, payload, socket) do
    Logger.info("Channel Connecting: #{email}")
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, payload}, socket}
  end
  def handle_in("load:data", payload, socket) do
    colleagues = App.Colleagues.list_users()
    |> (Enum.map &App.Colleagues.to_json/1)
    :ok = push socket, "data", %{:colleagues => colleagues}

    {:noreply, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (user:lobby).
  def handle_in("shout", payload, socket) do
    broadcast socket, "shout", payload
    {:noreply, socket}
  end

  def handle_out("new:colleague", payload, socket) do
    IO.inspect [:outward, "new:colleague", payload]
    push socket, "new:colleague", payload
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end

end
