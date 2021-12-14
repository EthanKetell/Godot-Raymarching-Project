extends Spatial
tool

var target
export(NodePath) var target_node setget set_target
func set_target(val):
	target = get_node_or_null(val)
	if target:
		update_params()

func update_params():
	var shader = target.material
	shader.set_shader_param("camera_position", $Camera.transform.origin)
	shader.set_shader_param("camera_rotation", $Camera.transform.basis)
