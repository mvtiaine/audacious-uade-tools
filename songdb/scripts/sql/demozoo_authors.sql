-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>

COPY (
    SELECT DISTINCT
        a.id AS releaser_id,
        a.name AS releaser_name,
        array_agg(DISTINCT b.name) as nicks,
        array_agg(DISTINCT c.name) as nick_variants
    FROM
        demoscene_releaser a
    LEFT JOIN demoscene_nick b
        ON b.releaser_id = a.id
    LEFT JOIN demoscene_nickvariant c
        ON c.nick_id = b.id
    WHERE
        a.is_group = false
    GROUP BY
        a.id
) TO '/tmp/demozoo_authors.tsv' WITH NULL AS '';
