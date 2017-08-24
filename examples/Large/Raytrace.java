// Equations are taken from
// http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtrace0.htm
// and I've looked at
// http://www.nobugs.org/developer/htrace/htrace.hs
// when writing this.

class Raytrace {

  public static void main(String[] args) {
    System.out.println((new Raytracer()).raytrace());
  }
}

class Vector {

  int x;
  int y;
  int z;

  public int getX() {
    return x;
  }

  public int getY() {
    return y;
  }

  public int getZ() {
    return z;
  }

  public int setX(int x1) {
    x = x1;
    return x;
  }

  public int setY(int y1) {
    y = y1;
    return y;
  }

  public int setZ(int z1) {
    z = z1;
    return z;
  }

  public int setXYZ(int x1, int y1, int z1) {
    x = x1;
    y = y1;
    z = z1;
    return 0;
  }

  public int set(Vector v) {
    x = v.getX();
    y = v.getY();
    z = v.getZ();
    return 0;
  }
}

class Ray {

  Vector r0;
  Vector dir;

  public Ray init() {
    r0 = new Vector();
    dir = new Vector();
    return this;
  }

  public Vector getBase() {
    return r0;
  }

  public Vector getDir() {
    return dir;
  }
}

class Scene {

  int sphereCount;
  int[] sphereX;
  int[] sphereY;
  int[] sphereZ;
  int[] sphereR;
  int[] sphereRed;
  int[] sphereGreen;
  int[] sphereBlue;
  int[] sphereRefl;
  int lightCount;
  int[] lightsX;
  int[] lightsY;
  int[] lightsZ;

  public int init(int f) {
	 // f is a scaling factor so that 
	 // 1*f represents 0.1 and -12*f represents -1.2
	 // (approximately)

    sphereCount = 4;
    sphereX = new int[sphereCount];
    sphereY = new int[sphereCount];
    sphereZ = new int[sphereCount];
    sphereR = new int[sphereCount];
    sphereRed = new int[sphereCount];
    sphereGreen = new int[sphereCount];
    sphereBlue = new int[sphereCount];
    sphereRefl = new int[sphereCount];

    sphereX[0] = f*(0-7);
    sphereY[0] = 0;
    sphereZ[0] = 0;
    sphereR[0] = f*11;
    sphereRed[0] = f*7;
    sphereGreen[0] = f*1;
    sphereBlue[0] = 0;
    sphereRefl[0] = f*2;

    sphereX[1] = f*12;
    sphereY[1] = 0;
    sphereZ[1] = f*3;
    sphereR[1] = f*6;
    sphereRed[1] = f*0;
    sphereGreen[1] = f*0;
    sphereBlue[1] = f*4;
    sphereRefl[1] = f*3;

    sphereX[2] = f*5;
    sphereY[2] = f*7;
    sphereZ[2] = f*(0 - 4);
    sphereR[2] = f*5;
    sphereRed[2] = 0;
    sphereGreen[2] = f*5;
    sphereBlue[2] = 0;
    sphereRefl[2] = f*1;

    sphereX[3] = f*0;
    sphereY[3] = f*50;
    sphereZ[3] = f*20;
    sphereR[3] = f*40;
    sphereRed[3] = f*3;
    sphereGreen[3] = f*3;
    sphereBlue[3] = f*3;
    sphereRefl[3] = f*2;

    lightCount = 1;
    lightsX = new int[lightCount];
    lightsY = new int[lightCount];
    lightsZ = new int[lightCount];

    lightsX[0] = f*20;
    lightsY[0] = f*10;
    lightsZ[0] = 0 - f*50;

    return 0;
  }

  public int getSphereCount() {
    return sphereCount;
  }

  public int getSphereX(int i) {
    return sphereX[i];
  }

  public int getSphereY(int i) {
    return sphereY[i];
  }

  public int getSphereZ(int i) {
    return sphereZ[i];
  }

  public int getSphereR(int i) {
    return sphereR[i];
  }

  public int getSphereRed(int i) {
    return sphereRed[i];
  }

  public int getSphereGreen(int i) {
    return sphereGreen[i];
  }

  public int getSphereBlue(int i) {
    return sphereBlue[i];
  }

  public int getSphereRefl(int i) {
    return sphereRefl[i];
  }

  public int getLightCount() {
    return lightCount;
  }

  public int getLightsX(int i) {
    return lightsX[i];
  }

  public int getLightsY(int i) {
    return lightsY[i];
  }

  public int getLightsZ(int i) {
    return lightsZ[i];
  }

}

class Raytracer {

  int factor;
  int sqrfactor;
  Scene scene;
  // return values from closestIntersection
  int intersectionTime;
  int intersectionSphere;
  // return values from reflectedRay
  Vector out;
  Vector normal;

  public int init() {
    int c;
    factor = 4096;
    sqrfactor = 64;
    scene = new Scene();
    c = scene.init(factor/10);
    out = new Vector();
    normal = new Vector();
    return 0;
  }

  public int mul(int x, int y) {
    return (x * y)/ factor;
  }
  
  // quick'n'dirty approximation of square root
  public int sqrt(int x) {
    int i;
    int i0;
    i0 = x;
    i = (x +1)/2;
    while (0 < i && i < i0) {
      i0 = i;
      i = (i +  x/ i)/2;
    }
    return i;
  }

  public int sqMag(Vector v) {
    return this.mul(v.getX(), v.getX()) + this.mul(v.getY(), v.getY()) + this.mul(v.getZ(), v.getZ());
  }

  public int dot(Vector v1, Vector v2) {
    return this.mul(v1.getX(), v2.getX()) + this.mul(v1.getY(), v2.getY()) + this.mul(v1.getZ(), v2.getZ());
  }

  public int normalise(Vector v) {
    int mag;
    int y;
    int x;
    int d;
    int z;
    int yy;

    mag = sqrfactor * this.sqrt(this.sqMag(v));
    d = v.setX(v.getX() * factor / mag);
    d = v.setY(v.getY() * factor / mag);
    d = v.setZ(v.getZ() * factor / mag);
    return mag;
  }

  // Gives return value in intersectionTime and intersectionSphere 
  // (in order to avoid allocating a new object in the absence of GC)
  public int closestIntersection(Ray r) {
    int a;
    int b;
    int c;
    int discriminant;
    int t1;
    int i;
    int n;
    intersectionTime = 10000 * factor;
    intersectionSphere = 0 - 1;
    i = 0;
    n = scene.getSphereCount();
    while (i < n) {
      a = this.sqMag(r.getDir());
      b = 2 * (this.mul(r.getDir().getX(), (r.getBase().getX() - scene.getSphereX(i)))
              + this.mul(r.getDir().getY(), (r.getBase().getY() - scene.getSphereY(i)))
              + this.mul(r.getDir().getZ(), (r.getBase().getZ() - scene.getSphereZ(i))));
      c = this.mul((r.getBase().getX() - scene.getSphereX(i)), (r.getBase().getX() - scene.getSphereX(i)))
              + this.mul(r.getBase().getY() - scene.getSphereY(i), r.getBase().getY() - scene.getSphereY(i))
              + this.mul(r.getBase().getZ() - scene.getSphereZ(i), r.getBase().getZ() - scene.getSphereZ(i))
              - this.mul(scene.getSphereR(i), scene.getSphereR(i));
      discriminant = this.mul(b, b) - 4 * this.mul(a, c);
      if (!(discriminant < 0)) {
        t1 = (0 - b + sqrfactor * this.sqrt(discriminant)) / 2;
        if (280 < t1 && t1 < intersectionTime) {
          intersectionTime = t1;
          intersectionSphere = i;
        } else {
        }
        t1 = (0 - b - sqrfactor * this.sqrt(discriminant)) / 2;
        if (280 < t1 && t1 < intersectionTime) {
          intersectionTime = t1;
          intersectionSphere = i;
        } else {
        }
      } else {
      }
      i = i + 1;
    }
    return 0;
  }

  // Gives return value in out and normal
  // (in order to avoid allocating a new object in the absence of GC)
  public int reflectedRay(Ray in, int sphere) {
    int k;
    int d;
    d = normal.setX(in.getBase().getX() - scene.getSphereX(sphere));
    d = normal.setY(in.getBase().getY() - scene.getSphereY(sphere));
    d = normal.setZ(in.getBase().getZ() - scene.getSphereZ(sphere));
    d = this.normalise(normal);
    k = (0 - 2) * this.dot(normal, in.getDir());
    d = out.setX(this.mul(normal.getX(), k) + in.getDir().getX());
    d = out.setY(this.mul(normal.getY(), k) + in.getDir().getY());
    d = out.setZ(this.mul(normal.getZ(), k) + in.getDir().getZ());
    d = this.normalise(out);
    return 0;
  }

  public int raytrace() {

    int red;
    int green;
    int blue;
    int colourfactor;
    int mag;
    int s;
    Ray ray;
    Ray intersection;
    Ray rayToLight;
    Ray rayFromLight;
    Ray rayToViewer;
    Vector v;
    int level;
    int c;
    int d;
    int i;
    int x;
    int y;
    int time;
    int window;

    c = this.init();
    ray = new Ray().init();
    intersection = new Ray().init();
    rayToLight = new Ray().init();
    rayFromLight = new Ray().init();
    rayToViewer = new Ray().init();

    System.out.print((char)80);
    System.out.println(3);
    System.out.println(400);
    System.out.println(400);
    System.out.println(256);

    y = 0;
    while (y < 400) {
      x = 0;
      while (x < 400) {
        red = 0;
        green = 0;
        blue = 0;
        colourfactor = factor; 
        d = ray.getBase().setX((x * factor) / 100 - 2 * factor);
        d = ray.getBase().setY((y * factor)/ 100 - 2 * factor);
        d = ray.getBase().setZ((0 - 2) * factor);
        d = ray.getDir().setX(0);
        d = ray.getDir().setY(0);
        d = ray.getDir().setZ(factor);
        level = 0;

        while (level < 4) {
          c = this.closestIntersection(ray);
          if (intersectionSphere < 0) {
            level = 10;
          } else {
            d = intersection.getBase().setXYZ(
                     ray.getBase().getX() + this.mul(intersectionTime, ray.getDir().getX()),
                     ray.getBase().getY() + this.mul(intersectionTime, ray.getDir().getY()),
                     ray.getBase().getZ() + this.mul(intersectionTime, ray.getDir().getZ()));
            d = intersection.getDir().set(ray.getDir());
            d = rayToViewer.getBase().set(intersection.getBase());
            d = rayToViewer.getDir().setXYZ(ray.getBase().getX() - intersection.getBase().getX(),
                     ray.getBase().getY() - intersection.getBase().getY(),
                     ray.getBase().getZ() - intersection.getBase().getZ());
            c = this.reflectedRay(intersection, intersectionSphere);
            d = ray.getBase().set(intersection.getBase());
            d = ray.getDir().set(out);
            s = intersectionSphere;
            i = 0;
            red = red + this.mul(4*colourfactor /10, scene.getSphereRed(s));
            green = green + this.mul(4*colourfactor /10, scene.getSphereGreen(s));
            blue = blue + this.mul(4*colourfactor /10, scene.getSphereBlue(s));
            while (i < scene.getLightCount()) {
              d = rayToLight.getBase().set(intersection.getBase());
              d = rayToLight.getDir().setX(scene.getLightsX(i) - intersection.getBase().getX());
              d = rayToLight.getDir().setY(scene.getLightsY(i) - intersection.getBase().getY());
              d = rayToLight.getDir().setZ(scene.getLightsZ(i) - intersection.getBase().getZ());
              mag = this.normalise(rayToLight.getDir());
              if (0 < mag) {
                c = this.closestIntersection(rayToLight);
                if (intersectionSphere < 0) {
                  c = this.dot(rayToLight.getDir(), normal);
                  red = red + this.mul(this.mul(c, colourfactor), scene.getSphereRed(s));
                  green = green + this.mul(this.mul(c, colourfactor), scene.getSphereGreen(s));
                  blue = blue + this.mul(this.mul(c, colourfactor), scene.getSphereBlue(s));
                  d = rayFromLight.getBase().set(intersection.getBase());
                  d = rayFromLight.getDir().setX(intersection.getBase().getX()- scene.getLightsX(i));
                  d = rayFromLight.getDir().setY(intersection.getBase().getY()- scene.getLightsY(i));
                  d = rayFromLight.getDir().setZ(intersection.getBase().getZ()- scene.getLightsZ(i));
                  d = this.normalise(rayFromLight.getDir());
                  c = this.reflectedRay(rayFromLight, s);
                  c = this.normalise(rayToViewer.getDir());
                  c = this.normalise(out);
                  c = this.dot(rayToViewer.getDir(), out);
                  if (c < 0) { c = 0; } else {}
                  c = (c * c) / factor;
                  c = (c * c) / factor;
                  c = (c * c) / factor;
                  red = red + this.mul(this.mul(c, colourfactor), scene.getSphereRefl(s));
                  green = green + this.mul(this.mul(c, colourfactor), scene.getSphereRefl(s));
                  blue = blue + this.mul(this.mul(c, colourfactor), scene.getSphereRefl(s));
                } else {}
              } else {
              }
              i = i + 1;
            }
            colourfactor = this.mul(colourfactor, scene.getSphereRefl(s));
            level = level + 1;
          }
        }

        if (!(red < 0)) {
          System.out.println((red * 256)/factor);
        } else {
          System.out.println(0);
        }
        if (!(green < 0)) {
          System.out.println((green * 256)/factor);
        } else {
          System.out.println(0);
        }
        if (!(blue < 0)) {
          System.out.println((blue * 256)/factor);
        } else {
          System.out.println(0);
        }
        x = x + 1;
      }
      y = y + 1;
    }
    return 999;
  }
}
