#ifndef __GRAPHICS_HEADER__
#define __GRAPHICS_HEADER__

#define FONT_CHARACTER_SIZE 128

#include <string>

inline int next_p2(int a){
	int rval = 1;
	while (rval<a) rval <<= 1;
	return rval;
}

struct DrawGlyph {
public:
	int textureID;
	int advance;
	int left;
	int top;

	int textureWidth;
	int textureHeight;
	int bitmapWidth;
	int bitmapHeight;
};

class DrawFont {
public:
	DrawFont(std::string fontName, int h);
	~DrawFont();

	void DrawString(int x, int y, std::string text);

private:
	DrawGlyph fontGlyphs[FONT_CHARACTER_SIZE];
};


#endif // __GRAPHICS_HEADER__