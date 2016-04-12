{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Mapnik (
  demo
) where

import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import qualified Language.C.Inline     as C
import qualified Language.C.Inline.Cpp as C


C.context (C.baseCtx <> C.cppCtx <> C.bsCtx)

C.include "<mapnik/map.hpp>"
C.include "<mapnik/layer.hpp>"
C.include "<mapnik/rule.hpp>"
C.include "<mapnik/feature_type_style.hpp>"
C.include "<mapnik/symbolizer.hpp>"
C.include "<mapnik/text/placements/dummy.hpp>"
C.include "<mapnik/text/text_properties.hpp>"
C.include "<mapnik/text/formatting/text.hpp>"
C.include "<mapnik/datasource_cache.hpp>"
C.include "<mapnik/font_engine_freetype.hpp>"
C.include "<mapnik/agg_renderer.hpp>"
C.include "<mapnik/expression.hpp>"
C.include "<mapnik/color_factory.hpp>"
C.include "<mapnik/image_util.hpp>"
C.include "<mapnik/unicode.hpp>"
C.include "<mapnik/save_map.hpp>"
C.include "<mapnik/cairo_io.hpp>"
C.include "<iostream>"

pluginDir :: BS.ByteString
pluginDir = DEFAULT_INPUT_PLUGIN_DIR

fontDir :: BS.ByteString
fontDir = DEFAULT_FONT_DIR

demo :: IO ()
demo = do
  ret <- [C.block| int {
    try {
        using namespace mapnik;
        const std::string srs_lcc="+proj=lcc +ellps=GRS80 +lat_0=49 +lon_0=-95 +lat+1=49 +lat_2=77 +datum=NAD83 +units=m +no_defs";
        const std::string srs_merc="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over";

        const std::string plugin_dir($bs-ptr:pluginDir, $bs-len:pluginDir);
        const std::string font_dir($bs-ptr:fontDir, $bs-len:fontDir);

        std::cout << plugin_dir << std::endl;
        std::cout << font_dir << std::endl;

        std::cout << " running demo ... \n";
        datasource_cache::instance().register_datasources(plugin_dir);
        freetype_engine::register_font(font_dir + "/dejavu-fonts-ttf-2.34/ttf/DejaVuSans.ttf");

        Map m(800,600);
        m.set_background(parse_color("white"));
        m.set_srs(srs_merc);
        // create styles

        // Provinces (polygon)
        feature_type_style provpoly_style;
        provpoly_style.reserve(2); // prevent reallocation and copying in add_rule
        {
            rule r;
            r.set_filter(parse_expression("[NAME_EN] = 'Ontario'"));
            {
                polygon_symbolizer poly_sym;
                put(poly_sym, keys::fill, color(250, 190, 183));
                r.append(std::move(poly_sym));
            }
            provpoly_style.add_rule(std::move(r));
        }
        {
            rule r;
            r.set_filter(parse_expression("[NOM_FR] = 'Québec'"));
            {
                polygon_symbolizer poly_sym;
                put(poly_sym, keys::fill, color(217, 235, 203));
                r.append(std::move(poly_sym));
            }
            provpoly_style.add_rule(std::move(r));
        }
        m.insert_style("provinces", std::move(provpoly_style));

        // Provinces (polyline)
        feature_type_style provlines_style;
        {
            rule r;
            {
                line_symbolizer line_sym;
                put(line_sym,keys::stroke,color(0,0,0));
                put(line_sym,keys::stroke_width,1.0);
                dash_array dash;
                dash.emplace_back(8,4);
                dash.emplace_back(2,2);
                dash.emplace_back(2,2);
                put(line_sym,keys::stroke_dasharray,dash);
                r.append(std::move(line_sym));
            }
            provlines_style.add_rule(std::move(r));
        }
        m.insert_style("provlines", std::move(provlines_style));

        // Drainage
        feature_type_style qcdrain_style;
        {
            rule r;
            r.set_filter(parse_expression("[HYC] = 8"));
            {
                polygon_symbolizer poly_sym;
                put(poly_sym, keys::fill, color(153, 204, 255));
                r.append(std::move(poly_sym));
            }
            qcdrain_style.add_rule(std::move(r));
        }
        m.insert_style("drainage", std::move(qcdrain_style));

        // Roads 3 and 4 (The "grey" roads)
        feature_type_style roads34_style;
        {
            rule r;
            r.set_filter(parse_expression("[CLASS] = 3 or [CLASS] = 4"));
            {
                line_symbolizer line_sym;
                put(line_sym,keys::stroke,color(171,158,137));
                put(line_sym,keys::stroke_width,2.0);
                put(line_sym,keys::stroke_linecap,ROUND_CAP);
                put(line_sym,keys::stroke_linejoin,ROUND_JOIN);
                r.append(std::move(line_sym));
            }
            roads34_style.add_rule(std::move(r));
        }
        m.insert_style("smallroads", std::move(roads34_style));

        // Roads 2 (The thin yellow ones)
        feature_type_style roads2_style_1;
        {
            rule r;
            r.set_filter(parse_expression("[CLASS] = 2"));
            {
                line_symbolizer line_sym;
                put(line_sym,keys::stroke,color(171,158,137));
                put(line_sym,keys::stroke_width,4.0);
                put(line_sym,keys::stroke_linecap,ROUND_CAP);
                put(line_sym,keys::stroke_linejoin,ROUND_JOIN);
                r.append(std::move(line_sym));
            }
            roads2_style_1.add_rule(std::move(r));
        }
        m.insert_style("road-border", std::move(roads2_style_1));

        feature_type_style roads2_style_2;
        {
            rule r;
            r.set_filter(parse_expression("[CLASS] = 2"));
            {
                line_symbolizer line_sym;
                put(line_sym,keys::stroke,color(255,250,115));
                put(line_sym,keys::stroke_width,2.0);
                put(line_sym,keys::stroke_linecap,ROUND_CAP);
                put(line_sym,keys::stroke_linejoin,ROUND_JOIN);
                r.append(std::move(line_sym));
            }
            roads2_style_2.add_rule(std::move(r));
        }
        m.insert_style("road-fill", std::move(roads2_style_2));

        // Roads 1 (The big orange ones, the highways)
        feature_type_style roads1_style_1;
        {
            rule r;
            r.set_filter(parse_expression("[CLASS] = 1"));
            {
                line_symbolizer line_sym;
                put(line_sym,keys::stroke,color(188,149,28));
                put(line_sym,keys::stroke_width,7.0);
                put(line_sym,keys::stroke_linecap,ROUND_CAP);
                put(line_sym,keys::stroke_linejoin,ROUND_JOIN);
                r.append(std::move(line_sym));
            }
            roads1_style_1.add_rule(std::move(r));
        }
        m.insert_style("highway-border", std::move(roads1_style_1));

        feature_type_style roads1_style_2;
        {
            rule r;
            r.set_filter(parse_expression("[CLASS] = 1"));
            {
                line_symbolizer line_sym;
                put(line_sym,keys::stroke,color(242,191,36));
                put(line_sym,keys::stroke_width,5.0);
                put(line_sym,keys::stroke_linecap,ROUND_CAP);
                put(line_sym,keys::stroke_linejoin,ROUND_JOIN);
                r.append(std::move(line_sym));
            }
            roads1_style_2.add_rule(std::move(r));
        }
        m.insert_style("highway-fill", std::move(roads1_style_2));

        // Populated Places
        feature_type_style popplaces_style;
        {
            rule r;
            {
                text_symbolizer text_sym;
                text_placements_ptr placement_finder = std::make_shared<text_placements_dummy>();
                placement_finder->defaults.format_defaults.face_name = "DejaVu Sans Book";
                placement_finder->defaults.format_defaults.text_size = 10.0;
                placement_finder->defaults.format_defaults.fill = color(0,0,0);
                placement_finder->defaults.format_defaults.halo_fill = color(255,255,200);
                placement_finder->defaults.format_defaults.halo_radius = 1.0;
                placement_finder->defaults.set_format_tree(std::make_shared<mapnik::formatting::text_node>(parse_expression("[GEONAME]")));
                put<text_placements_ptr>(text_sym, keys::text_placements_, placement_finder);
                r.append(std::move(text_sym));
            }
            popplaces_style.add_rule(std::move(r));
        }

        m.insert_style("popplaces", std::move(popplaces_style));


        // layers
        // Provincial  polygons
        {
            parameters p;
            p["type"]="shape";
            p["file"]="data/boundaries";
            // p["encoding"]="latin1";

            layer lyr("Provinces");
            lyr.set_datasource(datasource_cache::instance().create(p));
            lyr.add_style("provinces");
            lyr.set_srs(srs_lcc);
            m.add_layer(lyr);
        }

        // Drainage
        {
            parameters p;
            p["type"]="shape";
            p["file"]="data/qcdrainage";
            layer lyr("Quebec Hydrography");
            lyr.set_datasource(datasource_cache::instance().create(p));
            lyr.set_srs(srs_lcc);
            lyr.add_style("drainage");
            m.add_layer(lyr);
        }

        {
            parameters p;
            p["type"]="shape";
            p["file"]="data/ontdrainage";
            layer lyr("Ontario Hydrography");
            lyr.set_datasource(datasource_cache::instance().create(p));
            lyr.set_srs(srs_lcc);
            lyr.add_style("drainage");
            m.add_layer(lyr);
        }

        // Provincial boundaries
        {
            parameters p;
            p["type"]="shape";
            p["file"]="data/boundaries_l";
            layer lyr("Provincial borders");
            lyr.set_srs(srs_lcc);
            lyr.set_datasource(datasource_cache::instance().create(p));
            lyr.add_style("provlines");
            m.add_layer(lyr);
        }

        // Roads
        {
            parameters p;
            p["type"]="shape";
            p["file"]="data/roads";
            layer lyr("Roads");
            lyr.set_srs(srs_lcc);
            lyr.set_datasource(datasource_cache::instance().create(p));
            lyr.add_style("smallroads");
            lyr.add_style("road-border");
            lyr.add_style("road-fill");
            lyr.add_style("highway-border");
            lyr.add_style("highway-fill");

            m.add_layer(lyr);
        }
        // popplaces
        {
            parameters p;
            p["type"]="shape";
            p["file"]="data/popplaces";
            // p["encoding"] = "latin1";
            layer lyr("Populated Places");
            lyr.set_srs(srs_lcc);
            lyr.set_datasource(datasource_cache::instance().create(p));
            lyr.add_style("popplaces");
            m.add_layer(lyr);
        }

        m.zoom_to_box(box2d<double>(-8024477.28459,5445190.38849,-7381388.20071,5662941.44855));

        image_rgba8 buf(m.width(),m.height());
        agg_renderer<image_rgba8> ren(m,buf);
        ren.apply();
        std::string msg("These maps have been rendered using AGG in the current directory:\n");
        save_to_file(buf,"demo.jpg","jpeg");
        msg += "- demo.jpg\n";
        save_to_file(buf,"demo.png","png");
        save_to_file(buf,"demo256.png","png8");
        msg += "- demo.png\n";
        msg += "- demo256.png\n";
        save_to_file(buf,"demo.tif","tiff");
        msg += "- demo.tif\n";
        msg += "Have a look!\n";
        std::cout << msg;

        // save map definition (data + style)
        save_map(m, "map.xml");
    }
    catch ( std::exception const& ex )
    {
        std::cerr << "### std::exception: " << ex.what() << std::endl;
        return EXIT_FAILURE;
    }
    catch ( ... )
    {
        std::cerr << "### Unknown exception." << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
    } |]
  print ret
