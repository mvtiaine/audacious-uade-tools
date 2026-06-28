-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>

COPY (
    SELECT DISTINCT CONCAT('https://defacto2.net/d/',a.parameter) FROM
        productions_production_types c,
        productions_productionlink a,
        productions_production b
    WHERE
        -- a.is_download_link = true AND
        a.link_class = 'Defacto2File'
        AND a.production_id = b.id
        AND c.production_id = b.id
        AND c.productiontype_id IN (
            SELECT id FROM productions_productiontype
            WHERE
                name NOT LIKE 'ANSI%' AND
                name NOT LIKE 'ASCII%' AND
                name NOT LIKE '%Graphics' AND
                name NOT LIKE '%Door' AND
                name NOT LIKE '%Model' AND
                name NOT LIKE '%Music' AND
                name NOT LIKE '%Papermag' AND
                name NOT LIKE '%Performance' AND
                name NOT LIKE '%Photo' AND
                name NOT LIKE '%Textmag'
        )
) TO '/tmp/demozoo_defacto2.tsv' WITH NULL AS '';

COPY (
    SELECT DISTINCT CONCAT('ftp://ftp.padua.org/pub/c64',a.parameter) FROM
        productions_production_types c,
        productions_productionlink a,
        productions_production b
    WHERE
        -- a.is_download_link = true AND
        a.link_class = 'PaduaOrgFile'
        AND a.production_id = b.id
        AND c.production_id = b.id
        AND c.productiontype_id IN (
            SELECT id FROM productions_productiontype
            WHERE
                name NOT LIKE 'ANSI%' AND
                name NOT LIKE 'ASCII%' AND
                name NOT LIKE '%Graphics' AND
                name NOT LIKE '%Door' AND
                name NOT LIKE '%Model' AND
                name NOT LIKE '%Music' AND
                name NOT LIKE '%Papermag' AND
                name NOT LIKE '%Performance' AND
                name NOT LIKE '%Photo' AND
                name NOT LIKE '%Textmag'
        )
) TO '/tmp/demozoo_padua.tsv' WITH NULL AS '';

COPY (
    SELECT DISTINCT CONCAT('https://ftp.untergrund.net',a.parameter) FROM
        productions_production_types c,
        productions_productionlink a,
        productions_production b
    WHERE
        -- a.is_download_link = true AND
        a.link_class = 'UntergrundFile'
        AND a.production_id = b.id
        AND c.production_id = b.id
        AND c.productiontype_id IN (
            SELECT id FROM productions_productiontype
            WHERE
                name NOT LIKE 'ANSI%' AND
                name NOT LIKE 'ASCII%' AND
                name NOT LIKE '%Graphics' AND
                name NOT LIKE '%Door' AND
                name NOT LIKE '%Model' AND
                name NOT LIKE '%Music' AND
                name NOT LIKE '%Papermag' AND
                name NOT LIKE '%Performance' AND
                name NOT LIKE '%Photo' AND
                name NOT LIKE '%Textmag'
        )
) TO '/tmp/demozoo_untergrund.tsv' WITH NULL AS '';

COPY (
    SELECT DISTINCT CONCAT('https://web.archive.org/web/',a.parameter) FROM
        productions_production_types c,
        productions_productionlink a,
        productions_production b
    WHERE
        a.is_download_link = true
        AND a.link_class = 'WaybackMachinePage'
        AND a.production_id = b.id
        AND c.production_id = b.id
        AND c.productiontype_id IN (
            SELECT id FROM productions_productiontype
            WHERE
                name NOT LIKE 'ANSI%' AND
                name NOT LIKE 'ASCII%' AND
                name NOT LIKE '%Graphics' AND
                name NOT LIKE '%Door' AND
                name NOT LIKE '%Model' AND
                name NOT LIKE '%Music' AND
                name NOT LIKE '%Papermag' AND
                name NOT LIKE '%Performance' AND
                name NOT LIKE '%Photo' AND
                name NOT LIKE '%Textmag'
        )
) TO '/tmp/demozoo_waybackmachine.tsv' WITH NULL AS '';

COPY (
    SELECT DISTINCT a.parameter FROM
        productions_production_types c,
        productions_productionlink a,
        productions_production b
    WHERE
        a.is_download_link = true
        AND a.link_class = 'BaseUrl'
        AND NOT (
            a.parameter LIKE 'http%://amp.dascene.net/%'
            OR a.parameter LIKE 'http%://aminet.net/%'
            OR a.parameter LIKE 'http%://wt.exotica.org.uk/files/%'
            OR a.parameter LIKE 'http%://files.exotica.org.uk/?file=exotica/media/audio/UnExoticA/%'
            OR a.parameter LIKE 'http%://www.exotica.org.uk/download.php?file=media/audio/UnExoticA/%'
        )
        AND a.production_id = b.id
        AND c.production_id = b.id
        AND c.productiontype_id IN (
            SELECT id FROM productions_productiontype
            WHERE
                name NOT LIKE 'ANSI%' AND
                name NOT LIKE 'ASCII%' AND
                name NOT LIKE '%Graphics' AND
                name NOT LIKE '%Door' AND
                name NOT LIKE '%Model' AND
                name NOT LIKE '%Music' AND
                name NOT LIKE '%Papermag' AND
                name NOT LIKE '%Performance' AND
                name NOT LIKE '%Photo' AND
                name NOT LIKE '%Textmag'
        )
) TO '/tmp/demozoo_leftovers.tsv' WITH NULL AS '';
