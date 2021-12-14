extends ColorRect
tool

export var camera_speed := 1.0
export var camera_position:Vector3 = Vector3(0, 0, 5) setget set_camera_position
export var camera_rotation:Vector3 = Vector3.ZERO setget set_camera_rotation

var camera_basis:Basis

func set_aspect_ratio():
	self.material.set_shader_param("aspect_ratio", rect_size.x/rect_size.y)

func set_camera_position(val):
	camera_position = val
	self.material.set_shader_param("camera_position", camera_position)

func set_camera_rotation(val):
	camera_rotation = val
	camera_basis = Basis.IDENTITY
	camera_basis = camera_basis.rotated(camera_basis.z, val.z)
	camera_basis = camera_basis.rotated(camera_basis.x, val.x)
	camera_basis = camera_basis.rotated(camera_basis.y, val.y)
	
	self.material.set_shader_param("camera_rotation", camera_basis)

func _gui_input(event):
	if event is InputEventMouseMotion and Input.get_mouse_mode() == Input.MOUSE_MODE_CAPTURED:
		camera_basis = camera_basis.rotated(Vector3.RIGHT, event.relative.y/1000)
		camera_basis = camera_basis.rotated(camera_basis.y, event.relative.x/1000)
		self.material.set_shader_param("camera_rotation", camera_basis)
		
	if event is InputEventMouseButton and event.button_index == BUTTON_LEFT and event.pressed:
		if Input.get_mouse_mode() != Input.MOUSE_MODE_CAPTURED:
			Input.set_mouse_mode(Input.MOUSE_MODE_CAPTURED)
	if event is InputEventKey and event.scancode == KEY_ESCAPE and event.pressed:
		if Input.get_mouse_mode() == Input.MOUSE_MODE_CAPTURED:
			Input.set_mouse_mode(Input.MOUSE_MODE_VISIBLE)
	if event is InputEventMouseButton and event.button_index == BUTTON_WHEEL_UP:
		camera_speed = clamp(camera_speed * 1.1, .25, 5)
	if event is InputEventMouseButton and event.button_index == BUTTON_WHEEL_DOWN:
		camera_speed = clamp(camera_speed / 1.1, .25, 5)

func _process(delta):
	var motion = Vector3(
		int(Input.is_action_pressed("move_right")) - int(Input.is_action_pressed("move_left")),
		int(Input.is_action_pressed("move_up")) - int(Input.is_action_pressed("move_down")),
		int(Input.is_action_pressed("move_backward")) - int(Input.is_action_pressed("move_forward"))
		).normalized()

	self.camera_position += camera_basis.xform_inv(motion)*delta*camera_speed
	
func _ready():
	set_aspect_ratio()
	self.connect("item_rect_changed", self, "set_aspect_ratio")
