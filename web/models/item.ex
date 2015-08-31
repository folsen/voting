defmodule Voting.Item do
  use Voting.Web, :model

  schema "items" do
    field :description, :string
    field :item_type, :string
    field :points, :integer

    timestamps
  end

  @required_fields ~w(description item_type points)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end
end
