defmodule Voting.ItemView do
  use Voting.Web, :view

  def render("index.json", %{items: items}) do
    %{data: render_many(items, Voting.ItemView, "item.json")}
  end

  def render("show.json", %{item: item}) do
    %{data: render_one(item, Voting.ItemView, "item.json")}
  end

  def render("item.json", %{item: item}) do
    %{id: item.id,
      description: item.description,
      item_type: item.item_type,
      points: item.points}
  end
end
