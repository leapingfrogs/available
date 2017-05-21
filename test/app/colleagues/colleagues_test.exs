defmodule App.ColleaguesTest do
  use App.DataCase

  alias App.Colleagues

  describe "users" do
    alias App.Colleagues.User

    @valid_attrs %{email: "some email", location: "some location", name: "some name", timezone: "some timezone"}
    @update_attrs %{email: "some updated email", location: "some updated location", name: "some updated name", timezone: "some updated timezone"}
    @invalid_attrs %{email: nil, location: nil, name: nil, timezone: nil}

    def user_fixture(attrs \\ %{}) do
      {:ok, user} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Colleagues.create_user()

      user
    end

    test "list_users/0 returns all users" do
      user = user_fixture()
      assert Colleagues.list_users() == [user]
    end

    test "get_user!/1 returns the user with given id" do
      user = user_fixture()
      assert Colleagues.get_user!(user.id) == user
    end

    test "create_user/1 with valid data creates a user" do
      assert {:ok, %User{} = user} = Colleagues.create_user(@valid_attrs)
      assert user.email == "some email"
      assert user.location == "some location"
      assert user.name == "some name"
      assert user.timezone == "some timezone"
    end

    test "create_user/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Colleagues.create_user(@invalid_attrs)
    end

    test "update_user/2 with valid data updates the user" do
      user = user_fixture()
      assert {:ok, user} = Colleagues.update_user(user, @update_attrs)
      assert %User{} = user
      assert user.email == "some updated email"
      assert user.location == "some updated location"
      assert user.name == "some updated name"
      assert user.timezone == "some updated timezone"
    end

    test "update_user/2 with invalid data returns error changeset" do
      user = user_fixture()
      assert {:error, %Ecto.Changeset{}} = Colleagues.update_user(user, @invalid_attrs)
      assert user == Colleagues.get_user!(user.id)
    end

    test "delete_user/1 deletes the user" do
      user = user_fixture()
      assert {:ok, %User{}} = Colleagues.delete_user(user)
      assert_raise Ecto.NoResultsError, fn -> Colleagues.get_user!(user.id) end
    end

    test "change_user/1 returns a user changeset" do
      user = user_fixture()
      assert %Ecto.Changeset{} = Colleagues.change_user(user)
    end
  end
end
