{% for %}
{% else %}
{% endfor %}
{% if %}
{% elif %}
{% endif %}
{% block %}
{% endblock %}
{% extends %}
{% print %}
{% macro %}
{% endmacro %}
{% include %}
{% from %}
{% import %}
{% set %}
{% endset %}
{% with %}
{% endwith %}
{% autoescape %}
{% endautoescape %}
{% call %}
{% endcall %}
{% filter %}
{% endfilter %}
{% raw %}
{% endraw %}
{% trans %}
{% pluralize %}
{% endtrans %}
{% do %}
{% break %}
{% continue %}
{% debug %}
{% test %}
{% endtest %}
{% snapshot %}
{% endsnapshot %}
{% materialization %}
{% endmaterialization %}
{% docs %}
{% enddocs %}
{{ x | abs }}
{{ x | attr }}
{{ x | batch }}
{{ x | capitalize }}
{{ x | center }}
{{ x | count }}
{{ x | d }}
{{ x | default }}
{{ x | dictsort }}
{{ x | e }}
{{ x | escape }}
{{ x | filesizeformat }}
{{ x | first }}
{{ x | float }}
{{ x | forceescape }}
{{ x | format }}
{{ x | groupby }}
{{ x | indent }}
{{ x | int }}
{{ x | join }}
{{ x | last }}
{{ x | length }}
{{ x | list }}
{{ x | lower }}
{{ x | items }}
{{ x | map }}
{{ x | min }}
{{ x | max }}
{{ x | pprint }}
{{ x | random }}
{{ x | reject }}
{{ x | rejectattr }}
{{ x | replace }}
{{ x | reverse }}
{{ x | round }}
{{ x | safe }}
{{ x | select }}
{{ x | selectattr }}
{{ x | slice }}
{{ x | sort }}
{{ x | string }}
{{ x | striptags }}
{{ x | sum }}
{{ x | title }}
{{ x | trim }}
{{ x | truncate }}
{{ x | unique }}
{{ x | upper }}
{{ x | urlencode }}
{{ x | urlize }}
{{ x | wordcount }}
{{ x | wordwrap }}
{{ x | xmlattr }}
{{ x | tojson }}
{{ x is odd }}
{{ x is even }}
{{ x is divisibleby }}
{{ x is defined }}
{{ x is undefined }}
{{ x is filter }}
{{ x is test }}
{{ x is none }}
{{ x is boolean }}
{{ x is false }}
{{ x is true }}
{{ x is integer }}
{{ x is float }}
{{ x is lower }}
{{ x is upper }}
{{ x is string }}
{{ x is mapping }}
{{ x is number }}
{{ x is sequence }}
{{ x is iterable }}
{{ x is callable }}
{{ x is sameas }}
{{ x is escaped }}
{{ x is in }}
{{ x is eq }}
{{ x is equalto }}
{{ x is ne }}
{{ x is gt }}
{{ x is greaterthan }}
{{ x is ge }}
{{ x is lt }}
{{ x is lessthan }}
{{ x is le }}
{{ range }}
{{ dict }}
{{ lipsum }}
{{ cycler }}
{{ joiner }}
{{ namespace }}
{{ loop }}
{{ adapter }}
{{ as_bool }}
{{ as_native }}
{{ as_number }}
{{ builtins }}
{{ config }}
{{ dbt_version }}
{{ debug }}
{{ dispatch }}
{{ doc }}
{{ env_var }}
{{ exceptions }}
{{ execute }}
{{ flags }}
{{ fromjson }}
{{ fromyaml }}
{{ graph }}
{{ info_schema }}
{{ invocation_id }}
{{ local_md5 }}
{{ log }}
{{ model }}
{{ modules }}
{{ print }}
{{ project_name }}
{{ ref }}
{{ return }}
{{ run_query }}
{{ run_started_at }}
{{ schema }}
{{ schemas }}
{{ selected_resources }}
{{ set }}
{{ source }}
{{ statement }}
{{ target }}
{{ this }}
{{ thread_id }}
{{ tojson }}
{{ toyaml }}
{{ var }}
{{ zip }}
{{ dbt.type_bigint }}
{{ dbt.type_boolean }}
{{ dbt.type_float }}
{{ dbt.type_int }}
{{ dbt.type_numeric }}
{{ dbt.type_string }}
{{ dbt.type_timestamp }}
{{ dbt.current_timestamp }}
{{ dbt.except }}
{{ dbt.intersect }}
{{ dbt.array_append }}
{{ dbt.array_concat }}
{{ dbt.array_construct }}
{{ dbt.concat }}
{{ dbt.hash }}
{{ dbt.length }}
{{ dbt.position }}
{{ dbt.replace }}
{{ dbt.right }}
{{ dbt.split_part }}
{{ dbt.escape_single_quotes }}
{{ dbt.string_literal }}
{{ dbt.any_value }}
{{ dbt.bool_or }}
{{ dbt.listagg }}
{{ dbt.cast }}
{{ dbt.cast_bool_to_text }}
{{ dbt.safe_cast }}
{{ dbt.equals }}
{{ dbt.date }}
{{ dbt.dateadd }}
{{ dbt.datediff }}
{{ dbt.date_trunc }}
{{ dbt.last_day }}
{{ database_schemas }}
{{ results }}
{{ a and b }}
{{ a or b }}
{{ a not b }}
{{ a in b }}
{{ a is b }}
{{ a if b }}
{{ a else b }}
{{ true }}
{{ True }}
{{ false }}
{{ False }}
{{ none }}
{{ None }}
{% call statement("q", fetch_result=True) %}select 1{% endcall %}
{{ f(1) }} {{ 'a' ~ "b" }} {{ 1.5 + 3 }} {# note #} {{ shop.name }}
{{ dbt.concat(["a", "b"]) }} {{ x.dbt.concat }}
{% endfro %} {{ x | shout }} {{ x is odd_ish }}
