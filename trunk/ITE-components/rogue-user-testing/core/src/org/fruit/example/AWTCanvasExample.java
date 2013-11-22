/**************************************************************************************
*  Copyright (c) 2013, Universitat Politecnica de Valencia. All rights reserved.      *
*  This program and the accompanying materials are made available under the terms     *
*  of the 3-Clause BSD License which accompanies this distribution, and is available  *
*  at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these  *
*  results has received funding from the European Community`s Seventh Framework       *
*  Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.           *
**************************************************************************************/

/**
 *  @author Sebastian Bauersfeld
 */
package org.fruit.example;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import org.fruit.alayer.AWTCanvas;
import org.fruit.alayer.Color;
import org.fruit.alayer.Pen;
import org.fruit.alayer.Rect;

public class AWTCanvasExample {
	public static void main(String[] args) throws IOException, ClassNotFoundException{

		final int xres = 1024, yres = 768;
		AWTCanvas scrshot = AWTCanvas.fromScreenshot(Rect.from(0, 0, xres, yres));
		AWTCanvas pic = AWTCanvas.fromFile("/Users/guitest/Desktop/fruit_logo.png");
		//pic = AWTImage.FromScreenshot(new Rect(0, 0, xres, yres));
		
		scrshot.begin();
		int crop = 10;
		System.out.println(pic.width());
		pic.paint(scrshot, Rect.from(crop, crop, pic.width() - 2 * crop, pic.height() - 2 * crop), 
				Rect.from(0, 0, 200, 200));
		
		
		Pen p = Pen.newPen().setStrokeWidth(20).setColor(Color.from(0, 250, 0, 255)).build();
		//scrshot.rect(p, 0, 0, 100, 100, true);
		scrshot.line(p, 0, 0, 100, 100);

		scrshot.text(p, 400, 400, 0, "bla blubb 34 91 $ % ^ & *()");
		
		scrshot.end();
		
		scrshot.saveAsJpeg("/Users/guitest/Desktop/wuffinger.jpg", 0.5f);
		scrshot.saveAsPng("/Users/guitest/Desktop/wuffinger.png");

		for(int i = 0; i < 1; i++){
			saveImage(scrshot, "/Users/guitest/Desktop/brabutzinger");
			scrshot = loadImage("/Users/guitest/Desktop/brabutzinger");
			scrshot.saveAsJpeg("/Users/guitest/Desktop/wuffinger" + i + ".jpg", 0.5f);
		}
		

	}


	public static void saveImage(AWTCanvas image, String file) throws IOException{
		FileOutputStream fos = new FileOutputStream(new File(file));
		BufferedOutputStream bos = new BufferedOutputStream(fos);
		ObjectOutputStream oos = new ObjectOutputStream(bos);

		for(int i = 0; i < 1; i++){
			oos.writeObject(image);
			oos.reset();
		}

		oos.close();
		bos.close();
	}

	public static AWTCanvas loadImage(String file) throws IOException, ClassNotFoundException{
		FileInputStream fis = new FileInputStream(new File(file));
		BufferedInputStream bis = new BufferedInputStream(fis);
		ObjectInputStream ois = new ObjectInputStream(bis);

		AWTCanvas ret = (AWTCanvas)ois.readObject();
		ois.close();
		return ret;
	}

}
