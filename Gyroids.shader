shader_type canvas_item;

uniform float speed = 1.0;

uniform vec4 ambient_light_color:hint_color = vec4(1.0);
uniform float ambient_light_intensity:hint_range(0,1) = 0.1;
uniform vec4 sunlight_color:hint_color = vec4(1.0);
uniform vec3 sunlight_direction = vec3(0.0, -1.0, 0.0);
uniform float light_penumbra:hint_range(1, 128) = 4.0;
uniform float shadow_min_dist:hint_range(1, 16) = 1.0;
uniform float shadow_darkness:hint_range(0,1) = 1.0;

uniform vec4 sky_color:hint_color = vec4(0.769, 0.918, 1.0, 1.0);
uniform vec4 sun_color:hint_color = vec4(1.0);
uniform float sun_radius:hint_range(0,2) = .25;
uniform float sun_sharpness:hint_range(0, 1) = .1;

uniform sampler2D gyroid_color:hint_albedo;
uniform float gyroid_roughness:hint_range(0,1) = 0.0;

uniform int gyroid_layers = 3;
uniform float gyroid_frequency_change = 2.0;
uniform float gyroid_amplitude_change = 0.5;


uniform vec4 moon_albedo:hint_color = vec4(1.0);
uniform float moon_orbit_height = 1.5;
uniform float moon_radius = 0.25;
uniform float moon_smooth_size = 0.01;

uniform vec3 cutaway_pos = vec3(0.0);
uniform float cutaway_size = 1.0;

uniform float blend_size:hint_range(0,1) = 0.1;
// END SHAPE DEFINITIONS

uniform vec3 camera_position = vec3(0.0, 0.0, 5.0);
uniform mat3 camera_rotation = mat3(1.0);
uniform float focal_length = 1.0;

uniform int max_steps = 256;
uniform float step_mult:hint_range(0,1) = 1.0;
uniform float surface_distance = 0.001;
uniform float max_distance = 100.0;

uniform float aspect_ratio = 1.7777;
uniform float gamma = 2.2;

uvec3 murmurHash31(uint src) {
    const uint M = uint(1540483477);
    uvec3 h = uvec3(uint(1190494759), uint(2147483647), uint(3559788179));
    src *= M;
	src = src^(src>>uint(24));
	src *= M;
    h *= M;
	h = uvec3(h.x^src, h.y^src, h.z^src);
    h = h^uvec3(h.x>>uint(13), h.y>>uint(13), h.z>>uint(13));
	h *= M;
	h = h^uvec3(h.x>>uint(15), h.y>>uint(15), h.z>>uint(15));
    return h;
}

vec3 hash31(float src) {
    uvec3 h = murmurHash31(floatBitsToUint(src));
    return uintBitsToFloat(uvec3(
		h.x & uint(8388607) | uint(1065353216),
		h.y & uint(8388607) | uint(1065353216),
		h.z & uint(8388607) | uint(1065353216))) - 1.0;
}

uint murmurHash13(uvec3 src) {
    const uint M = uint(1540483477);
    uint h = uint(1190494759);
    src *= M;
	src = src^(src>>uint(24)); src *= M;
    h *= M; h = h^src.x; h *= M; h = h^src.y; h *= M; h = h^src.z;
    h = h^(h>>uint(13)); h *= M; h = h^(h>>uint(15));
    return h;
}

float hash13(vec3 src) {
    uint h = murmurHash13(floatBitsToUint(src));
    return uintBitsToFloat(h & uint(8388607) | uint(1065353216)) - 1.0;
}

uvec3 murmurHash33(uvec3 src) {
    const uint M = uint(1540483477);
    uvec3 h = uvec3(uint(1190494759), uint(2147483647), uint(3559788179));
    src *= M;
	src = src^(src>>uint(24));
	src *= M;
    h *= M;
	h = uvec3(h.x^src.x, h.y^src.x, h.z^src.x);
	h *= M;
	h = uvec3(h.x^src.y, h.y^src.y, h.z^src.y);
	h *= M;
	h = uvec3(h.x^src.z, h.y^src.z, h.z^src.z);
    h = h^(h>>uint(13));
	h *= M;
	h = h^(h>>uint(15));
    return h;
}

vec3 hash33(vec3 src) {
    uvec3 h = murmurHash33(floatBitsToUint(src));
    return uintBitsToFloat(uvec3(
		h.x & uint(8388607) | uint(1065353216),
		h.y & uint(8388607) | uint(1065353216),
		h.z & uint(8388607) | uint(1065353216)
		)) - 1.0;
}

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

///////////////////////////////
// Signed Distance Functions //
///////////////////////////////

float sdSphere(vec3 p, float r) {
	return length(p) - r;
}

float sdBox( vec3 p, vec3 b ) {
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

float sdRoundBox(vec3 p, vec3 b, float r) {
  return sdBox(p, b-r)-r;
}

float sdCappedCylinder( vec3 p, float h, float r ) {
  vec2 d = abs(vec2(length(p.xz),p.y)) - vec2(h,r);
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}

float sdCylinder( vec3 p, vec3 c )
{
  return length(p.xz-c.xy)-c.z;
}

float sdBoxFrame( vec3 p, vec3 b, float e ) {
  p = abs(p  )-b;
  vec3 q = abs(p+e)-e;
  return min(min(
      length(max(vec3(p.x,q.y,q.z),0.0))+min(max(p.x,max(q.y,q.z)),0.0),
      length(max(vec3(q.x,p.y,q.z),0.0))+min(max(q.x,max(p.y,q.z)),0.0)),
      length(max(vec3(q.x,q.y,p.z),0.0))+min(max(q.x,max(q.y,p.z)),0.0));
}

float sdPlane( vec3 p, vec3 n, float h )
{
  return dot(p,n) + h;
}

float sdGyroid(vec3 p) {
	return dot(sin(p), cos(p.zxy));
}

///////////////////////////////
///////// Operations //////////
///////////////////////////////

float opShell(float d, float thickness) {
	return abs(d) - thickness;
}

float opUnion( float d1, float d2 )
{
    return min(d1,d2);
}

float opDifference( float d1, float d2 )
{
    return max(d1,-d2);
}

float opIntersection( float d1, float d2 )
{
    return max(d1,d2);
}

float opSmoothUnion( float d1, float d2, float k )
{
    float h = max(k-abs(d1-d2),0.0);
    return min(d1, d2) - h*h*0.25/k;
}

float opSmoothIntersection( float d1, float d2, float k )
{
    float h = max(k-abs(d1-d2),0.0);
    return max(d1, d2) + h*h*0.25/k;
}

float opSmoothDifference( float d1, float d2, float k )
{
	return opSmoothIntersection(d1, -d2, k);
}

vec4 blend(vec4 d1, vec4 d2, float d) {
	return vec4(mix(d1.rgb, d2.rgb, clamp(abs(d1.w-d)/(abs(d1.w)+abs(d2.w)), 0.0, 1.0)), d);
}

/////////////////////////////
// Distance Field Function //
/////////////////////////////

float dist_field(vec3 pos, float time) {
	float result;
	
	{
		float dist = 0.0;
		float freq = 20.0;
		float scale = 1.0;
		for(int i = 0; i < gyroid_layers; i++) {
			dist += scale*(sdGyroid((pos+time*0.1)*freq)+mix(-1.0, 2.0, smoothstep(length(pos), 0.25, 0.5)))/freq;
			freq *= gyroid_frequency_change;
			scale *= gyroid_amplitude_change;
		}
		result = dist;
	}

	result = opSmoothIntersection(result, sdSphere(pos, 1.0), blend_size);
	result = opSmoothDifference(result, sdSphere(pos-cutaway_pos, cutaway_size), blend_size);
	
	{
		vec3 p = pos-vec3(sin(time), 0.0, cos(time))*moon_orbit_height;
		float moon = sdSphere(p, moon_radius);
		result = opSmoothUnion(result, moon, moon_smooth_size);
	}
	
	return result;
}

vec4 albedo_field(vec3 pos, float time) {
	vec4 result;
	
	{
		float dist = 0.0;
		float freq = 20.0;
		float scale = 1.0;
		for(int i = 0; i < gyroid_layers; i++) {
			dist += scale*(sdGyroid((pos+time*0.1)*freq)+mix(-1.0, 2.0, smoothstep(length(pos), 0.25, 0.5)))/freq;
			freq *= gyroid_frequency_change;
			scale *= gyroid_amplitude_change;
		}
		result = vec4(texture(gyroid_color, vec2(min(length(pos),1.0), 0.0)).rgb, dist);
	}

	result.w = opSmoothIntersection(result.w, sdSphere(pos, 1.0), blend_size);
	result.w = opSmoothDifference(result.w, sdSphere(pos-cutaway_pos, cutaway_size), blend_size);
	
	{
		vec3 p = pos-vec3(sin(time), 0.0, cos(time))*moon_orbit_height;
		vec4 moon = vec4(moon_albedo.rgb, sdSphere(p, moon_radius));
		result = blend(result, moon, opSmoothUnion(result.w, moon.w, moon_smooth_size));
	}
	
	return result;
}

vec3 get_normal(vec3 pos, float time) {
	float ep = surface_distance/2.0;
    vec2 e = vec2(1.0,-1.0)*0.5773;
    return normalize( e.xyy*dist_field(pos+e.xyy*ep, time) + 
					  e.yyx*dist_field( pos + e.yyx*ep, time) + 
					  e.yxy*dist_field( pos + e.yxy*ep, time) + 
					  e.xxx*dist_field( pos + e.xxx*ep, time));
}

vec3 sky_shader(vec3 dir, float time) {
	return mix(sky_color.rgb, sun_color.rgb, smoothstep(1.0-sun_radius, mix(1.0, 1.0-sun_radius, sun_sharpness), dot(dir, -normalize(sunlight_direction))));
}

vec4 single_ray(vec3 origin, vec3 dir, float time) {
	float dist = 0.0;
	for(int i = 0; i < max_steps; i++) {
		float df = dist_field(origin+dir*dist, time);
		dist += df*step_mult;
		if(abs(df) < (surface_distance*dist/max_distance) || dist > max_distance) {
			break;
		}
	}
	return vec4(origin+dir*dist, dist);
}

float soft_shadow(vec3 pos, float time) {
	float dist = shadow_min_dist*surface_distance;
	vec3 dir = normalize(-sunlight_direction);
	float nearest = 1.0;
	for(int i = 0; i < max_steps; i++) {
		float df = dist_field(pos+dir*dist, time);
		dist += df*step_mult;
		nearest = min(nearest, light_penumbra*df/dist);
		if(df < surface_distance) return 0.0;
		if(dist > max_distance) break;
	}
	return nearest;
}

float hard_shadow(vec3 pos, float time) {
	float dist = shadow_min_dist*surface_distance;
	vec3 dir = normalize(-sunlight_direction);
	for(int i = 0; i < max_steps; i++) {
		float df = dist_field(pos+dir*dist, time);
		dist += df*step_mult;
		if(df < surface_distance) return 0.0;
		if(dist > max_distance) break;
	}
	return 1.0;
}

vec3 calc_color(vec3 pos, vec3 view, vec3 normal, float time) {
	vec3 albedo = pow(albedo_field(pos, time).rgb, vec3(gamma));
	float diffuse = dot(normal, -normalize(sunlight_direction));
	float shadow = hard_shadow(pos, time);
	
	diffuse *= shadow;
	
	//vec3 h = sunli
	float specular = (1.0-pow(gyroid_roughness,2.0))*pow(max(dot(view, -reflect(view, normal)),0.0), 1.0/gyroid_roughness)*shadow;
	
	return albedo*diffuse+specular;
}

vec3 raymarch(vec3 origin, vec3 dir, float time) {
	vec4 ray = single_ray(origin, dir, time);
	if(ray.w > max_distance) return pow(sky_shader(dir, time), vec3(gamma));
	
	vec3 normal = get_normal(ray.xyz, time);
	
	return calc_color(ray.xyz, dir, normal, time);
}

void fragment() {
	vec3 origin = camera_position;
	vec3 dir = normalize(vec3((UV-0.5) * vec2(aspect_ratio, -1.0), -focal_length)*camera_rotation);
	COLOR.rgb = pow(raymarch(origin, dir, TIME*speed), vec3(1.0/gamma));
}