{% snapshot orders_snap %}
select * from {{ source('shop', 'orders') }}
{% endsnapshot %}
