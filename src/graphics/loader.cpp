#include "loader.h"
#define GLEW_STATIC
#include <gl/glew.h>
#include <GLFW/glfw3.h>
#include <string>

void StartupLoaderWindow(){
	glfwInit();

	GLFWmonitor* primary = glfwGetPrimaryMonitor();
	const GLFWvidmode* mainMode = glfwGetVideoMode(primary);


	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
	glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

	GLFWwindow* window = glfwCreateWindow(mainMode->width, mainMode->height, "Test", glfwGetPrimaryMonitor(), NULL);
	glfwMakeContextCurrent(window);

	glewExperimental = GL_TRUE;
	glewInit();


	glfwSwapInterval(1);

	while (!glfwWindowShouldClose(window)){
		// Run events

		glfwSwapBuffers(window);
		glfwPollEvents();
	}

	glfwDestroyWindow(window);
	glfwTerminate();
}