-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>

COPY (
SELECT DISTINCT
        a.soundtrack_id AS soundtrack_id,
        b.title as title,
        b.release_date_date AS mod_date,
        b.release_date_precision AS mod_date_precision,
        array_agg(DISTINCT e.name) AS authors,
        array_agg(DISTINCT c.id) AS prod_ids
    FROM
        productions_soundtracklink a
    INNER JOIN productions_production b
        ON a.soundtrack_id = b.id
    INNER JOIN productions_production c
        ON a.production_id = c.id
    LEFT JOIN productions_production_author_nicks d
        ON d.production_id = a.soundtrack_id
    LEFT JOIN demoscene_nick e
        ON e.id = d.nick_id
    GROUP BY
        a.soundtrack_id, b.title, b.release_date_date, b.release_date_precision
) TO '/tmp/demozoo_soundtracks.tsv' WITH NULL AS '';
