defmodule App.Repo.Migrations.CreateApp.Colleagues.User do
  use Ecto.Migration

  def change do
    create table(:colleagues_users, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :name, :string
      add :email, :string
      add :location, :string
      add :timezone, :string

      timestamps()
    end

  end
end
