init_params <- list()
init_params[['app_dir']] <- c('/Users/aaron/Documents/Research/R Package Tests/StereoMorph/Digitizing App/extdata/apps/digitizeImage')

init_params[['prev_wd']] <- c('/Users/aaron/Documents/Research/R Package Tests/StereoMorph/Digitizing App')

init_params[['img_name']] <- c('obj_a1_v1.JPG')

init_params[['img_size']] <- c('337909')

init_params[['auto_advance']] <- c('TRUE')

init_params[['img_file']] <- c('Images/obj_a1_v1.JPG')

init_params[['landmarks_file']] <- c('Landmarks/obj_a1_v1.txt')

init_params[['control_points_file']] <- c('Curve Control Points/obj_a1_v1.txt')

init_params[['landmark_color_blur']] <- c('blue')

init_params[['landmark_color_focus']] <- c('green')

init_params[['curve_color_blur']] <- c('purple')

init_params[['control_point_color_blur']] <- c('purple')

init_params[['control_point_color_focus']] <- c('red')

init_params[['landmark_radius']] <- c('4')

init_params[['control_point_radius']] <- c('4')

init_params[['marker_stroke_width']] <- c('1')

init_params[['landmarks_ref']] <- c('Landmark 1','Landmark 2','Landmark 3','Landmark 4','Curve 1 Start','Curve 1 End','Curve 2 Start','Curve 2 End','Curve 3 Start','Curve 3 End')

init_params[['curves_ref']][[1]] <- c('Curve 1','Curve 1 Start','Curve 1 End')
init_params[['curves_ref']][[2]] <- c('Curve 2','Curve 2 Start','Curve 2 End')
init_params[['curves_ref']][[3]] <- c('Curve 3','Curve 3 Start','Curve 3 End')

init_params[['unsaved_landmarks']] <- c('FALSE')

init_params[['unsaved_curves']] <- c('FALSE')

init_params[['prev_img']] <- c('FALSE')

init_params[['next_img']] <- c('TRUE')

init_params[['landmarks']][[1]] <- c('Landmark 1','608','257')
init_params[['landmarks']][[2]] <- c('Landmark 2','807','216')
init_params[['landmarks']][[3]] <- c('Landmark 3','964','355')

init_params[['control_points']][[1]] <- c('Curve 1','425','400','629','397','582','517')


json_string <- '{"app_dir":["/Users/aaron/Documents/Research/R Package Tests/StereoMorph/Digitizing App/extdata/apps/digitizeImage"], "prev_wd":["/Users/aaron/Documents/Research/R Package Tests/StereoMorph/Digitizing App"], "img_name":["obj_a1_v1.JPG"], "img_size":["337909"], "auto_advance":["TRUE"], "img_file":["Images/obj_a1_v1.JPG"], "landmarks_file":["Landmarks/obj_a1_v1.txt"], "control_points_file":["Curve Control Points/obj_a1_v1.txt"], "landmark_color_blur":["blue"], "landmark_color_focus":["green"], "curve_color_blur":["purple"], "control_point_color_blur":["purple"], "control_point_color_focus":["red"], "landmark_radius":["4"], "control_point_radius":["4"], "marker_stroke_width":["1"], "landmarks_ref":["Landmark 1", "Landmark 2", "Landmark 3", "Landmark 4", "Curve 1 Start", "Curve 1 End", "Curve 2 Start", "Curve 2 End", "Curve 3 Start", "Curve 3 End"], "curves_ref":[["Curve 1", "Curve 1 Start", "Curve 1 End"], ["Curve 2", "Curve 2 Start", "Curve 2 End"], ["Curve 3", "Curve 3 Start", "Curve 3 End"]],"unsaved_landmarks":["FALSE"], "unsaved_curves":["FALSE"], "prev_img":["FALSE"], "next_img":["TRUE"], "landmarks":[["Landmark 1", "608", "257"], ["Landmark 2", "807", "216"], ["Landmark 3", "964", "355"]],"control_points":[["Curve 1", "425", "400", "629", "397", "582", "517"]]}'
