extends ColorRect
tool

export var camera_spherical_pos:Vector3 = Vector3(0, 0, 5) setget set_camera_spherical_pos
export var camera_rotation:Vector3 = Vector3.ZERO setget set_camera_rotation

func set_aspect_ratio():
	self.material.set_shader_param("aspect_ratio", rect_size.x/rect_size.y)

func set_camera_spherical_pos(val):
	camera_spherical_pos = val
	update_shader()

func set_camera_rotation(val):
	camera_rotation = val
	update_shader()

func update_shader():
	var camera_pos := Vector3(0.0, 0.0, camera_spherical_pos.z);
	camera_pos = camera_pos.rotated(Vector3.RIGHT, camera_spherical_pos.x)
	camera_pos = camera_pos.rotated(Vector3.UP, camera_spherical_pos.y)
	
	var camera_basis := Basis.IDENTITY
	camera_basis = camera_basis.rotated(camera_basis.z, camera_rotation.z)
	camera_basis = camera_basis.rotated(camera_basis.x, camera_rotation.x-camera_spherical_pos.x)
	camera_basis = camera_basis.rotated(camera_basis.y, camera_rotation.y-camera_spherical_pos.y)
	
	self.material.set_shader_param("camera_position", camera_pos)
	self.material.set_shader_param("camera_rotation", camera_basis)

func _gui_input(event):
	if event is InputEventMouseButton and (event.button_index == BUTTON_LEFT or event.button_index == BUTTON_RIGHT or event.button_index == BUTTON_MIDDLE):
		Input.set_mouse_mode(Input.MOUSE_MODE_CAPTURED if event.pressed else Input.MOUSE_MODE_VISIBLE)
	if event is InputEventMouseMotion:
		if Input.is_mouse_button_pressed(BUTTON_LEFT):
			camera_spherical_pos.x -= event.relative.y/1000
			camera_spherical_pos.y -= event.relative.x/1000
		if Input.is_mouse_button_pressed(BUTTON_RIGHT):
			camera_rotation.x += event.relative.y/1000
			camera_rotation.y += event.relative.x/1000
		if Input.is_mouse_button_pressed(BUTTON_MIDDLE):
			camera_spherical_pos.z += event.relative.y/1000
	if event is InputEventMouseButton and event.button_index == BUTTON_WHEEL_UP:
		camera_spherical_pos.z = max(0.01, camera_spherical_pos.z/1.1)
	if event is InputEventMouseButton and event.button_index == BUTTON_WHEEL_DOWN:
		camera_spherical_pos.z = max(0.01, camera_spherical_pos.z*1.1)
	update_shader()
	
func _ready():
	set_aspect_ratio()
	self.connect("item_rect_changed", self, "set_aspect_ratio")
