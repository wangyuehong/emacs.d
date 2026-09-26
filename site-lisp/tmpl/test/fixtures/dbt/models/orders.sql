{{ config(materialized='tabled') }}
{# don't -- keep the host state #}
-- checked by is_positive
select
  id,
  {{ cents_to_usd('amount') }} as amount,
  '{{ var("region") }}' as region
from {{ ref('orders_snap') }}
where status = 'paid' and {{ "a/*" }} is not null
