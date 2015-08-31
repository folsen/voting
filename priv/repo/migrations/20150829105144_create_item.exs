defmodule Voting.Repo.Migrations.CreateItem do
  use Ecto.Migration

  def change do
    create table(:items) do
      add :description, :string
      add :item_type, :string
      add :points, :integer

      timestamps
    end

  end
end
