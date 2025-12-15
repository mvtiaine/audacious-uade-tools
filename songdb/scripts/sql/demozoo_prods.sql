-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

COPY (
SELECT DISTINCT
        a.id AS id,
        a.release_date_date AS prod_date,
        a.release_date_precision AS prod_date_precision,
        a.title AS prod,
        array_agg(DISTINCT c.name) AS prod_platforms,
        array_agg(DISTINCT e.name) AS prod_publishers,
        array_agg(DISTINCT n.name) FILTER (WHERE p.category = 'Music') AS music_authors,
--        array_agg(DISTINCT f.original_url) AS image_urls,
        j.name AS party,
        h.shown_date_date AS party_date,
        h.shown_date_precision AS party_date_precision,
        array_agg(DISTINCT k.name) AS production_type
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
        k.name NOT LIKE '%Textmag'
    GROUP BY
        a.id,
        a.release_date_date,
        a.release_date_precision,
        a.title,
        j.name,
        h.shown_date_date,
        h.shown_date_precision
) TO '/tmp/demozoo_prods.tsv' WITH NULL AS '';
