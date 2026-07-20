-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright (C) 2025-2026 Matti Tiainen <mvtiaine@cc.hut.fi>

CREATE OR REPLACE FUNCTION decode_url(p varchar) RETURNS varchar AS $$
BEGIN
    RETURN(SELECT convert_from(CAST(E'\\x' || string_agg(CASE WHEN length(r.m[1]) = 1 THEN encode(convert_to(r.m[1], 'SQL_ASCII'), 'hex') ELSE substring(r.m[1] from 2 for 2) END, '') AS bytea), 'UTF8')
    FROM regexp_matches($1, '%[0-9a-f][0-9a-f]|.', 'gi') AS r(m));
EXCEPTION WHEN OTHERS THEN
    RAISE NOTICE 'invalid url %', $1;
    RETURN $1;
END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;

COPY (
SELECT DISTINCT
        a.id AS id,
        a.release_date_date AS prod_date,
        a.release_date_precision AS prod_date_precision,
        a.title AS prod,
        array_agg(DISTINCT c.name) AS prod_platforms,
        array_agg(DISTINCT e.name) AS prod_publishers,
        array_agg(DISTINCT n.name) FILTER (WHERE p.category = 'Music') AS prod_music_authors,
--        array_agg(DISTINCT f.original_url) AS image_urls,
        j.name AS party,
        h.shown_date_date AS party_shown_date,
        h.shown_date_precision AS party_shown_date_precision,
        i.start_date_date AS party_start_date,
        i.start_date_precision AS party_start_date_precision,
        array_agg(DISTINCT k.name) AS production_type,
        array_agg(DISTINCT q.soundtrack_id) AS soundtrack_ids,
        o.link_class AS link_class,
        decode_url(o.parameter) AS path
    FROM
        productions_production a
    INNER JOIN productions_production_types b
        ON b.production_id = a.id
    INNER JOIN productions_productiontype k
        ON k.id = b.productiontype_id
    LEFT JOIN productions_production_platforms d
        ON d.production_id = a.id
    LEFT JOIN platforms_platform c
        ON c.id = d.platform_id
    LEFT JOIN productions_production_author_nicks m
        ON m.production_id = a.id
    LEFT JOIN demoscene_nick e
        ON e.id = m.nick_id
    LEFT JOIN productions_credit p
        ON p.production_id = a.id
    LEFT JOIN demoscene_nick n
        ON n.id = p.nick_id
    LEFT JOIN productions_screenshot f
        ON f.production_id = a.id
    LEFT JOIN parties_competitionplacing g
        ON g.production_id = a.id
    LEFT JOIN parties_competition h
        ON h.id = g.competition_id
    LEFT JOIN parties_party i
        ON i.id = h.party_id
    LEFT JOIN parties_partyseries j
        ON j.id = i.party_series_id
    LEFT JOIN productions_soundtracklink q
        ON q.production_id = a.id
    LEFT JOIN productions_productionlink o
        ON o.production_id = a.id
    WHERE
        k.name NOT LIKE 'ANSI%' AND
        k.name NOT LIKE 'ASCII%' AND
        k.name NOT LIKE '%Graphics' AND
        k.name NOT LIKE '%Door' AND
        k.name NOT LIKE '%Model' AND
        k.name NOT LIKE '%Music' AND
        k.name NOT LIKE '%Papermag' AND
        k.name NOT LIKE '%Performance' AND
        k.name NOT LIKE '%Photo' AND
        k.name NOT LIKE '%Textmag' AND
        o.is_download_link IS TRUE
    GROUP BY
        a.id,
        a.release_date_date,
        a.release_date_precision,
        a.title,
        j.name,
        h.shown_date_date,
        h.shown_date_precision,
        i.start_date_date,
        i.start_date_precision,
        o.link_class,
        o.parameter

) TO '/tmp/demozoo_prods.tsv' WITH NULL AS '';
