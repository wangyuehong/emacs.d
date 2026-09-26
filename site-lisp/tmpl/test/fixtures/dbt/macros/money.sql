{% macro cents_to_usd(column) %}
  ({{ column }} / 100)::numeric(16, 2)
{% endmacro %}
