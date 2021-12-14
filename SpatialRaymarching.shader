shader_type spatial;
render_mode  depth_test_disable;

const float surface_distance = 0.001;

uniform vec3 light_direction = vec3(0.0, -1.0, 0.0);

uniform sampler2D planet_1_color:hint_albedo;
uniform float metallic:hint_range(0,1) = 0.0;
uniform float roughness:hint_range(0,1) = 1.0;

uniform float terrain_frequency = 1.0;
uniform float terrain_scale = 0.1;
uniform vec3 terrain_offset = vec3(0.0);

uniform float mountain_frequency = 10.0;
uniform float mountain_scale = 0.1;
uniform float mountain_smoothness = 0.1;
uniform float mountain_altitude_min:hint_range(-1,1) = 0.1;
uniform float mountain_altitude_max:hint_range(-1,1) = 0.5;

uniform vec3 planet_1_pos = vec3(0.0);
uniform float planet_1_size = 1.0;


uniform int ray_max_steps:hint_range(0, 1024) = 128;
uniform float ray_max_distance:hint_range(0, 1024) = 128;

vec4 permute(vec4 x){return mod(((x*34.0)+1.0)*x, 289.0);}
vec4 taylorInvSqrt(vec4 r){return 1.79284291400159 - 0.85373472095314 * r;}

float snoise(vec3 v){ 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //  x0 = x0 - 0. + 0.0 * C 
  vec3 x1 = x0 - i1 + 1.0 * C.xxx;
  vec3 x2 = x0 - i2 + 2.0 * C.xxx;
  vec3 x3 = x0 - 1. + 3.0 * C.xxx;

// Permutations
  i = mod(i, 289.0 ); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients
// ( N*N points uniformly over a square, mapped onto an octahedron.)
  float n_ = 1.0/7.0; // N=7
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z *ns.z);  //  mod(p,N*N)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
}

vec3 sky_shader(vec3 rd) {
	return rd;
}

float height_map(vec3 pos) {
	return 1.0;
}

float smooth_abs(float x, float s) {
    return sqrt(x*x+s);
}

float sdSphere(vec3 pos, float radius) {
	return length(pos)-radius;
}

vec4 planet_1(vec3 pos) {
	vec3 local_pos = pos-planet_1_pos;
	float terrain_height = 0.0;
	terrain_height += snoise(terrain_offset+normalize(local_pos)*terrain_frequency);
	terrain_height += smoothstep(mountain_altitude_min, mountain_altitude_max, terrain_height)*mountain_scale*(0.5-smooth_abs(snoise(normalize(local_pos)*mountain_frequency), mountain_smoothness));
	
	vec3 color = texture(planet_1_color, vec2(terrain_height, 0.0)).rgb;
	
	return vec4(color, sdSphere(local_pos, planet_1_size*(1.0+terrain_height*terrain_scale)));
}

vec4 distance_field(vec3 pos) {

	vec4 result = planet_1(pos);
	return result;
}

vec3 get_normal(vec3 pos) {
	float d = distance_field(pos).w;
	vec2 offset = vec2(surface_distance, 0.0);
	vec3 normal = vec3(
		distance_field(pos + offset.xyy).w,
		distance_field(pos + offset.yxy).w,
		distance_field(pos + offset.yyx).w
	);
	return normalize(normal-d);
}

vec4 raymarch(vec3 ro, vec3 rd) {
	float dist = 0.0;
	vec4 df;
	for(int i = 0; i < ray_max_steps; i++) {
		df = distance_field(ro+rd*dist);
		dist += df.w;
		if(dist > ray_max_distance || abs(df.w) < surface_distance) {
			break;
		}
	}
	return vec4(mix(sky_shader(rd), df.rgb, step(dist, ray_max_distance)), dist);
}

void fragment() {
	vec4 view = INV_PROJECTION_MATRIX * vec4(vec3(SCREEN_UV, texture(DEPTH_TEXTURE, SCREEN_UV).x) * 2.0 - 1.0, 1.0);
	view.xyz /= view.w;
	float depth = -view.z;
	
	vec3 ro = (CAMERA_MATRIX * vec4(0.0, 0.0, 0.0, 1.0)).xyz;
	vec3 rd = normalize(((CAMERA_MATRIX * vec4(VERTEX, 1.0)).xyz) - ro);
	vec4 ray = raymarch(ro, rd);
	if(ray.w > ray_max_distance || ray.w > depth) {
		discard;
	}
	vec3 pos = ro+rd*ray.w;
	vec3 norm = get_normal(pos);
	ALBEDO.rgb = ray.rgb;//*vec3(dot(-normalize(light_direction), norm));
	
	NORMAL = (vec4(norm, 0.0)*CAMERA_MATRIX).xyz;
	ROUGHNESS = roughness;
	SPECULAR = 0.5;
	METALLIC = metallic;
}