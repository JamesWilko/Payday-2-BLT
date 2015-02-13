#include "graphics.h"
#define GLEW_STATIC
#include <gl/glew.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

DrawFont::DrawFont(std::string fontPath, int h){
	FT_Library ftLib;
	FT_Init_FreeType(&ftLib);

	FT_Face mainFace;
	FT_New_Face(ftLib, fontPath.c_str(), 0, &mainFace);
	FT_Set_Char_Size(mainFace, h << 6, h << 6, 96, 96);

	unsigned int* textureIDs = new unsigned int[FONT_CHARACTER_SIZE];
	glGenTextures(FONT_CHARACTER_SIZE, textureIDs);

	for (unsigned char i = 0; i < FONT_CHARACTER_SIZE; i++){
		FT_Load_Glyph(mainFace, FT_Get_Char_Index(mainFace, i), FT_LOAD_RENDER);

		FT_Bitmap& bitmap = mainFace->glyph->bitmap;

		int btWidth = bitmap.width;
		int btHeight = bitmap.rows;

		int txWidth = next_p2(btWidth);
		int txHeight = next_p2(btHeight);

		unsigned char* pixelData = new unsigned char[txWidth * txHeight];
		for (int y = 0; y < txHeight; y++){
			for (int x = 0; x < txWidth; x++){
				pixelData[y * txWidth + x] = (x >= btWidth || y >= btHeight) ? 0 : mainFace->glyph->bitmap.buffer[y * btWidth + x];
			}
		}

		glBindTexture(GL_TEXTURE_2D, textureIDs[i]);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

		glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, txWidth, txHeight, 0, GL_ALPHA, GL_UNSIGNED_BYTE, pixelData);

		delete[] pixelData;

		fontGlyphs[i].advance = mainFace->glyph->advance.x >> 6;
		fontGlyphs[i].bitmapHeight = btHeight;
		fontGlyphs[i].bitmapWidth = btWidth;
		fontGlyphs[i].left = mainFace->glyph->bitmap_left;
		fontGlyphs[i].top = mainFace->glyph->bitmap_top;
		fontGlyphs[i].textureID = textureIDs[i];
		fontGlyphs[i].textureWidth = txWidth;
		fontGlyphs[i].textureHeight = txHeight;
	}

	delete[] textureIDs;

	FT_Done_Face(mainFace);
	FT_Done_FreeType(ftLib);
}