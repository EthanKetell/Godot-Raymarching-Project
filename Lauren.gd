extends ColorRect
tool

func set_aspect():
	var aspect = get_rect().size.x/get_rect().size.y
	material.set_shader_param("aspect_ratio", aspect)

func _ready():
	set_aspect()
