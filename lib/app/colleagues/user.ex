defmodule App.Colleagues.User do
  use Ecto.Schema
  import Ecto.Changeset
  alias App.Colleagues.User


  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "colleagues_users" do
    field :email, :string
    field :location, :string
    field :name, :string
    field :timezone, :string

    timestamps()
  end

  @doc false
  def changeset(%User{} = user, attrs) do
    user
    |> cast(attrs, [:name, :email, :location, :timezone])
    |> validate_required([:name, :email, :location, :timezone])
  end
end
