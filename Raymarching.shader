shader_type canvas_item;

uniform int num_reflections:hint_range(0,16) = 0;
uniform float reflectiveness:hint_range(0,1);

uniform vec4 light_color:hint_color = vec4(1.0);
uniform vec3 light_direction = vec3(0.0, -1.0, 0.0);
uniform float light_penumbra:hint_range(1, 128) = 4.0;
uniform float shadow_min_dist:hint_range(0, 128) = 0.1;

uniform vec4 sky_color:hint_color = vec4(0.769, 0.918, 1.0, 1.0);
uniform vec4 ground_albedo:hint_color = vec4(1.0);

uniform vec3 rbox1_pos = vec3(0.0,0.0,0.0);
uniform vec3 rbox1_size = vec3(1.0, 1.0, 1.5);
uniform float rbox1_radius = 0.1;
uniform vec4 rbox1_albedo:hint_color = vec4(1.0);

// END SHAPE DEFINITIONS

uniform vec3 camera_position = vec3(0.0, 0.0, 5.0);
uniform mat3 camera_rotation = mat3(1.0);
uniform float focal_length = 1.0;

uniform int MAX_STEPS = 256;
uniform float aspect_ratio = 1.7777;
uniform float surface_distance = 0.001;
uniform float max_distance = 100.0;

float sdSphere(vec3 p, float r) {
	return length(p) - r;
}

float sdRoundBox(vec3 p, vec3 b, float r) {
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0) - r;
}

vec4 union_sharp(vec4 lhs, vec4 rhs) {
	return (lhs.w < rhs.w)?lhs:rhs;
}

vec4 intersection_sharp(vec4 lhs, vec4 rhs) {
	return (lhs.w > rhs.w)?lhs:rhs;
}

vec4 difference_sharp(vec4 lhs, vec4 rhs) {
	return intersection_sharp(lhs, rhs*vec4(1., 1., 1., -1.));
}

vec4 dist_field(vec3 pos, float time) {
	vec4 result = vec4(ground_albedo.rgb, pos.y+1.0);
	
	vec4 temp = intersection_sharp(vec4(rbox1_albedo.rgb, sdRoundBox(rbox1_pos-pos, rbox1_size, rbox1_radius)), vec4(vec3(0.0, 0.0, 1.0), sdSphere(pos, 1.1*abs(sin(time)))));
	
	result = union_sharp(result, temp);
	return result;
}

vec3 get_normal(vec3 pos, float time) {
	float dist = dist_field(pos, time).w;
	vec2 offset = vec2(surface_distance, 0.0);
	vec3 normal = vec3(
		dist_field(pos-offset.xyy, time).w,
		dist_field(pos-offset.yxy, time).w,
		dist_field(pos-offset.yyx, time).w);
	return normalize(dist-normal);
}

vec3 sky_shader(vec3 dir, float time) {
	return smoothstep(sky_color.rgb, vec3(1.0), clamp(vec3(dot(-light_direction, dir)), 0.0, 1.0));
}

vec4 single_ray(vec3 origin, vec3 dir, float time) {
	float dist = 0.0;
	vec4 field = dist_field(origin, time);
	for(int i = 0; i < MAX_STEPS; i++) {
		dist += field.w;
		field = dist_field(origin+dir*dist, time);
		if(abs(field.w) < surface_distance || dist > max_distance) {
			break;
		}
	}
	if(dist > max_distance) {
		return vec4(sky_shader(dir, time), dist);
	} else {
		return vec4(field.rgb, dist);
	}
}

vec3 get_light(vec3 pos, float time) {
	float dist = shadow_min_dist;
	vec3 dir = normalize(-light_direction);
	vec4 field;
	float nearest = 1.0;
	for(int i = 0; i < MAX_STEPS; i++) {
		field = dist_field(pos+dir*dist, time);
		dist += field.w;
		nearest = min(nearest, light_penumbra*field.w/dist);
		if(field.w < surface_distance) return vec3(0.0);
		if(dist > max_distance) break;
	}
	return nearest*light_color.rgb*dot(normalize(-light_direction), get_normal(pos, time));
}

vec3 raymarch(vec3 origin, vec3 dir, int num_bounces, float time) {
	vec4 ray = single_ray(origin, dir, time);
	vec3 pos = origin+dir*ray.w;
	vec3 norm = get_normal(pos, time);
	vec3 light = get_light(pos, time);
	return ray.rgb*light;
}

void fragment() {
	vec3 origin = camera_position;
	vec3 dir = normalize(vec3((UV-0.5) * vec2(aspect_ratio, -1.0), -focal_length))*camera_rotation;
	COLOR.rgb = raymarch(origin, dir, num_reflections, TIME);
}