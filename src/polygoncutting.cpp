#include <Rcpp.h>
#include <string>
#include <fstream>
#include <cmath>
#include <iostream>
#include <vector>
#include <ctime>
#include <chrono>
#include <limits>

/*
 This is a single-file version of the code, for use with Rcpp.
 See https://github.com/mathiasisaksen/polygon-cutting for better structured code.
 */

// [[Rcpp::plugins("cpp11")]]

// polygon.h

const double EPS = std::numeric_limits<double>::epsilon();
const double PI_DOUBLE = 3.14159265358979323846;
const double INF_DOUBLE = std::numeric_limits<double>::infinity();

bool isZero(double a);
double dmod(double number, double modulus);

class Vector;

struct Point
{
  double x, y;
  Point() : x(0), y(0) { ; }
  Point(double x, double y) : x(x), y(y) { ; }
  Point(Vector v);
};

class Vector
{
private:
  double x, y;

public:
  Vector() : x(0), y(0) { ; }
  Vector(double x, double y) : x(x), y(y) { ; }
  Vector(Point p) : x(p.x), y(p.y) { ; }
  Vector operator=(Point p);
  friend Vector operator*(double a, const Vector v);
  friend Vector operator+(Vector v1, Vector v2);
  friend Vector operator-(Vector v1, Vector v2);
  const double getX() { return x; }
  const double getY() { return y; }
  const double getLength() { return std::sqrt(std::pow(x, 2) + std::pow(y, 2)); }
  friend std::ostream &operator<<(std::ostream &out, const Vector &v);
};

double crossProduct(Vector v1, Vector v2);
double dotProduct(Vector v1, Vector v2);

class Line
{
private:
  Vector start, direction;

public:
  Line() : start(0, 0), direction(0, 0) { ; }
  Line(Point p1, Point p2);
  Line(Vector start, Vector direction) : start(start), direction(direction) { ; }
  Line(Point start, Vector direction) : start(Vector(start)), direction(direction) { ; }
  Vector getStart() { return start; }
  Vector getDirection() { return direction; }
};

double distancePointToLine(Point &p, Line &l);
std::ostream &operator<<(std::ostream &out, Line &l);

class Intersection
{
private:
  Point position;
  double time1;
  double time2;
  int index;

public:
  Intersection(Point position, double time1, double time2) : position(position), time1(time1), time2(time2) { ; }
  Point getPosition() { return position; }
  double getTime1() const { return time1; }
  double getTime2() const { return time2; }
  void setIndex(int index) { this->index = index; }
  int getIndex() const { return index; }
};

std::ostream &operator<<(std::ostream &out, Intersection &i);
bool operator<(const Intersection &lhs, const Intersection &rhs);
Intersection computeLineIntersection(Line l1, Line l2);

struct AABB
{
  double xMin, xMax, yMin, yMax;
  AABB() : xMin(INF_DOUBLE), xMax(-INF_DOUBLE), yMin(INF_DOUBLE), yMax(-INF_DOUBLE) { ; }
  AABB(double xMin, double xMax, double yMin, double yMax) : xMin(xMin), xMax(xMax), yMin(yMin), yMax(yMax) { ; }
};

class Polygon
{
private:
  std::vector<Point> points;
  Point centroid;
  double area;
  AABB aabb;
  void computeAreaAndCentroid();
  void computeAABB();

public:
  Polygon(std::vector<Point> points);
  std::vector<Point> getPoints() { return points; }
  Point getCentroid() { return centroid; }
  double getArea() { return area; }
  int getNumberOfPoints() { return points.size(); }
  bool isPolygonCCW();
  bool isPointInPolygon(Point point);
  bool isConvex();
  Point sampleUniform();
};

std::ostream &operator<<(std::ostream &out, Polygon &polygon);

template <class T>
std::ostream &operator<<(std::ostream &out, std::vector<T> &vector);

template <class T>
std::vector<T> getSubVector(std::vector<T> vector, int start, int end);

std::vector<Polygon> splitPolygon(Polygon polygon, Point cutPoint, double cutDirection);
std::vector<Polygon> splitPolygonRecursive(Polygon polygon, Point cutPoint, double cutDirection);
std::vector<Polygon> splitPolygonHelper(Polygon polygon, Line cutLine, double minTime = -INF_DOUBLE);

// polygon.cpp

bool isZero(double a)
{
  return std::abs(a) < EPS;
}

double dmod(double number, double modulus)
{
  return number - std::floor(number / modulus) * modulus;
}

template <class T>
std::ostream &operator<<(std::ostream &out, std::vector<T> &vector)
{
  for (int i = 0; i < vector.size(); i++)
  {
    out << i << ": " << vector[i] << std::endl;
  }
  return out;
}

template <class T>
std::vector<T> getSubVector(std::vector<T> vector, int start, int end)
{
  int n = vector.size();
  if (start >= end)
  {
    end += n;
  }
  std::vector<T> subVector;
  subVector.reserve(end - start + 1);

  for (int i = start; i <= end; i++)
  {
    subVector.push_back(vector[i % n]);
  }
  return (subVector);
}

Point::Point(Vector v)
{
  this->x = v.getX();
  this->y = v.getY();
}

Vector Vector::operator=(Point p)
{
  return Vector(p.x, p.y);
}

Vector operator*(double a, Vector v)
{
  return Vector(a * v.getX(), a * v.getY());
}

Vector operator+(Vector v1, Vector v2)
{
  return Vector(v1.getX() + v2.getX(), v1.getY() + v2.getY());
}

Vector operator-(Vector v1, Vector v2)
{
  return Vector(v1.getX() - v2.getX(), v1.getY() - v2.getY());
}

std::ostream &operator<<(std::ostream &out, const Vector &v)
{
  return out << "(" << v.x << ", " << v.y << ")";
}

double crossProduct(Vector v1, Vector v2)
{
  return v1.getX() * v2.getY() - v1.getY() * v2.getX();
}

double dotProduct(Vector v1, Vector v2)
{
  return v1.getX() * v2.getX() + v1.getY() * v2.getY();
}

Line::Line(Point start, Point end)
{
  this->start = Vector(start);
  this->direction = Vector(end) - Vector(start);
}

double distancePointToLine(Point &q, Line &l)
{
  Vector k = Vector(q) - l.getStart();
  Vector r = l.getDirection();
  return std::sqrt(std::pow(k.getLength(), 2) - std::pow(dotProduct(k, r) / r.getLength(), 2));
}

Intersection computeLineIntersection(Line l1, Line l2)
{
  // Approach from https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
  Vector p = l1.getStart();
  Vector r = l1.getDirection();
  Vector q = l2.getStart();
  Vector s = l2.getDirection();
  double cp = crossProduct(r, s);
  double time1 = crossProduct(q - p, s) / cp;
  double time2 = crossProduct(q - p, r) / cp;
  Point position = p + time1 * r;
  Intersection result(position, time1, time2);
  return result;
}

std::ostream &operator<<(std::ostream &out, Line &l)
{
  return out << l.getStart() << " + " << l.getDirection() << "*t";
}

std::ostream &operator<<(std::ostream &out, Intersection &i)
{
  return out << "Position: " << i.getPosition() << " Time 1: " << i.getTime1() << " Time 2: " << i.getTime2() << " Index: " << i.getIndex() << std::endl;
}

bool operator<(const Intersection &lhs, const Intersection &rhs)
{
  return lhs.getTime1() < rhs.getTime1();
}

void Polygon::computeAreaAndCentroid()
{
  double signedArea = 0;
  double cx = 0;
  double cy = 0;
  for (int i = 0, j = 1; i < points.size(); i++, j = (i + 1) % points.size())
  {
    signedArea += 0.5 * (points[i].x * points[j].y - points[j].x * points[i].y);
    cx += (points[i].x + points[j].x) * (points[i].x * points[j].y - points[j].x * points[i].y);
    cy += (points[i].y + points[j].y) * (points[i].x * points[j].y - points[j].x * points[i].y);
  }
  this->area = std::abs(signedArea);
  this->centroid = Point(cx / (6 * signedArea), cy / (6 * signedArea));
}

void Polygon::computeAABB()
{
  for (int i = 0; i < points.size(); i++)
  {
    double x = points[i].x;
    double y = points[i].y;
    aabb.xMin = std::min(x, aabb.xMin);
    aabb.xMax = std::max(x, aabb.xMax);
    aabb.yMin = std::min(y, aabb.yMin);
    aabb.yMax = std::max(y, aabb.yMax);
  }
}

Polygon::Polygon(std::vector<Point> points)
{
  this->points = points;
  computeAreaAndCentroid();
  computeAABB();
}

bool Polygon::isPointInPolygon(Point point)
{
  double x = point.x;
  double y = point.y;

  bool isInPolygon = false;
  int numVertices = points.size();
  for (int i = 0, j = numVertices - 1; i < numVertices; j = i++)
  {
    Point a = points[i];
    Point b = points[j];

    if (a.y > b.y)
    {
      a = points[j];
      b = points[i];
    }

    if (((y > a.y) != (y > b.y)) && ((x - a.x) * (b.y - a.y) < (y - a.y) * (b.x - a.x)))
    {
      isInPolygon = !isInPolygon;
    }
  }

  return isInPolygon;
}

bool Polygon::isPolygonCCW()
{
  int n = points.size();
  // Find vertex with lowest x-coordinate (and highest y-coordinate if there are ties)
  Point minXPoint = points[0];
  int minXIndex = 0;
  for (int i = 1; i < n; i++)
  {
    Point curPoint = points[i];

    if ((curPoint.x < minXPoint.x) ||
        ((curPoint.x == minXPoint.x) && (curPoint.y > minXPoint.y)))
    {
      minXPoint = curPoint;
      minXIndex = i;
    }
  }
  int prevIndex = minXIndex == 0 ? n - 1 : minXIndex - 1;
  int nextIndex = minXIndex == n - 1 ? 0 : minXIndex + 1;

  Point prevPoint = points[prevIndex];
  Point nextPoint = points[nextIndex];

  double det = (minXPoint.x - prevPoint.x) * (nextPoint.y - prevPoint.y) -
    (nextPoint.x - prevPoint.x) * (minXPoint.y - prevPoint.y);
  return det > 0;
}

bool Polygon::isConvex()
{
  // A polygon is convex if every internal angle is less than or equal to pi
  int n = points.size();
  bool isCCW = isPolygonCCW();
  int sign = isCCW ? 1 : -1;
  for (int i = 0; i < n; i++)
  {
    Point curPoint = points[i];
    Point prevPoint = points[i == 0 ? n - 1 : i - 1];
    Point nextPoint = points[i == n - 1 ? 0 : i + 1];

    double firstAngle = std::atan2(prevPoint.y - curPoint.y,
                                   prevPoint.x - curPoint.x);
    double secondAngle = std::atan2(nextPoint.y - curPoint.y,
                                    nextPoint.x - curPoint.x);
    double internalAngle = dmod(sign * (firstAngle - secondAngle), 2 * PI_DOUBLE);
    if (internalAngle > PI_DOUBLE)
    {
      return false;
    }
  }
  return true;
}

Point Polygon::sampleUniform()
{
  Point sample;
  do
  {
    sample.x = Rcpp::runif(1, aabb.xMin, aabb.xMax)[0];
    sample.y = Rcpp::runif(1, aabb.yMin, aabb.yMax)[0];
  } while (!isPointInPolygon(sample));
  return sample;
}

std::ostream &operator<<(std::ostream &out, Polygon &polygon)
{
  out << "Centroid: " << polygon.getCentroid() << std::endl;
  out << "Area: " << polygon.getArea() << std::endl;
  std::vector<Point> points = polygon.getPoints();
  for (int i = 0; i < points.size(); i++)
  {
    out << points[i] << std::endl;
  }
  return out;
}

std::vector<Polygon> splitPolygon(Polygon polygon, Point cutPoint, double cutDirection)
{
  Vector cutVector(std::cos(cutDirection), std::sin(cutDirection));
  Line cutLine(cutPoint, cutVector);

  std::vector<Point> points = polygon.getPoints();
  std::vector<Point> pointsLeft, pointsRight;

  std::vector<int> posIndices, negIndices;
  std::vector<Point> posPoints, negPoints;
  int curPosIndex(0), curNegIndex(0);
  for (int i = 0; i < points.size(); i++)
  {
    Point p = points[i];
    double cp = crossProduct(Vector(p) - Vector(cutPoint), cutVector);
    if (cp > 0)
    {
      if (posIndices.size() == 0)
      {
        posIndices.push_back(i);
        posPoints.push_back(p);
      }
      else
      {
        if (posIndices[curPosIndex - 1] != i - 1)
        {
          curPosIndex = 0;
        }
        posIndices.insert(posIndices.begin() + curPosIndex, i);
        posPoints.insert(posPoints.begin() + curPosIndex, p);
      }
      curPosIndex++;
    }
    else
    {
      if (negIndices.size() == 0)
      {
        negIndices.push_back(i);
        negPoints.push_back(p);
      }
      else
      {
        if (negIndices[curNegIndex - 1] != i - 1)
        {
          curNegIndex = 0;
        }
        negIndices.insert(negIndices.begin() + curNegIndex, i);
        negPoints.insert(negPoints.begin() + curNegIndex, p);
      }
      curNegIndex++;
    }
  }
  Line line1 = Line(posPoints[0], negPoints[negPoints.size() - 1]);
  Intersection intersection1 = computeLineIntersection(cutLine, line1);
  Intersection intersection2 = computeLineIntersection(cutLine, Line(negPoints[0], posPoints[posPoints.size() - 1]));
  posPoints.insert(posPoints.begin(), intersection1.getPosition());
  posPoints.push_back(intersection2.getPosition());
  negPoints.insert(negPoints.begin(), intersection2.getPosition());
  negPoints.push_back(intersection1.getPosition());
  std::vector<Polygon> result;
  result.push_back(Polygon(posPoints));
  result.push_back(Polygon(negPoints));
  return result;
}

std::vector<Polygon> splitPolygonRecursive(Polygon polygon, Point cutPoint, double cutDirection)
{
  Vector cutVector(std::cos(cutDirection), std::sin(cutDirection));
  Line cutLine(cutPoint, cutVector);
  std::vector<Polygon> cutPolygons = splitPolygonHelper(polygon, cutLine);

  return cutPolygons;
}

std::vector<Polygon> splitPolygonHelper(Polygon polygon, Line cutLine, double minTime)
{
  int n = polygon.getNumberOfPoints();
  std::vector<Intersection> intersections;
  std::vector<Point> points = polygon.getPoints();

  for (int i = 0, j = 1; i < n; i++, j = (i + 1) % n)
  {
    Line edge(points[i], points[j]);
    Intersection intersection = computeLineIntersection(cutLine, edge);
    intersection.setIndex(i);

    // An intersection is valid if it intersects the edge (0 <= time2 <= 1),
    // and happens later than any earlier intersections
    if (intersection.getTime2() > 0 && intersection.getTime2() < 1 &&
        intersection.getTime1() > minTime)
    {
      intersections.push_back(intersection);
    }
  }
  if (intersections.size() < 2)
  {
    std::vector<Polygon> result;
    result.push_back(polygon);
    return result;
  }
  // Sort intersections by the order they occur along the line
  std::sort(intersections.begin(), intersections.end());
  Intersection intersection1 = intersections[0];
  Intersection intersection2 = intersections[1];
  double nextMinTime = intersection2.getTime1();

  int index1 = intersection1.getIndex();
  int index2 = intersection2.getIndex();

  // Pick out one point too many at start and end, and replace with intersection points
  std::vector<Point> points1 = getSubVector(polygon.getPoints(), index1, (index2 + 1) % n);
  points1[0] = intersection1.getPosition();
  points1[points1.size() - 1] = intersection2.getPosition();

  std::vector<Point> points2 = getSubVector(polygon.getPoints(), index2, (index1 + 1) % n);
  points2[0] = intersection2.getPosition();
  points2[points2.size() - 1] = intersection1.getPosition();

  Polygon polygon1(points1);
  Polygon polygon2(points2);
  std::vector<Polygon> cutPolygons1 = splitPolygonHelper(polygon1, cutLine, nextMinTime);
  std::vector<Polygon> cutPolygons2 = splitPolygonHelper(polygon2, cutLine, nextMinTime);
  cutPolygons1.insert(cutPolygons1.end(), cutPolygons2.begin(), cutPolygons2.end());
  return cutPolygons1;
}

// cut.h

class Cut
{
private:
  std::vector<Polygon> polygons;

public:
  Cut() : polygons() { ; }
  Cut(std::vector<Polygon> polygons) : polygons(polygons) { ; }
  Cut(Polygon polygon);
  int getNumberOfPolygons() { return polygons.size(); }
  int getTotalNumberOfPoints();
  Polygon getPolygon(int i) { return polygons[i]; }
  void addPolygon(Polygon polygon);
  void addPolygons(std::vector<Polygon> &polygons);
  void replacePolygon(Polygon newPolygon, int replaceIndex);
};

Cut cutEveryPolygon(Cut &cut, bool useCentroid = true);
void cutSinglePolygon(Cut &cut, int polygonIndex);

// cut.cpp

Cut::Cut(Polygon polygon)
{
  polygons.push_back(polygon);
}

int Cut::getTotalNumberOfPoints()
{
  int total = 0;
  for (int i = 0; i < this->getNumberOfPolygons(); i++)
  {
    total += this->getPolygon(i).getNumberOfPoints();
  }
  return total;
}

void Cut::addPolygon(Polygon polygon)
{
  this->polygons.push_back(polygon);
}

void Cut::addPolygons(std::vector<Polygon> &newPolygons)
{
  for (int i = 0; i < newPolygons.size(); i++)
  {
    this->polygons.push_back(newPolygons[i]);
  }
}

void Cut::replacePolygon(Polygon newPolygon, int replaceIndex)
{
  polygons[replaceIndex] = newPolygon;
}

Cut cutEveryPolygon(Cut &cut, bool useCentroid)
{
  Cut newCut;
  for (int i = 0; i < cut.getNumberOfPolygons(); i++)
  {
    Polygon currentPolygon = cut.getPolygon(i);
    Point cutPoint;
    if (useCentroid)
    {
      cutPoint = currentPolygon.getCentroid();
    }
    else
    {
      cutPoint = currentPolygon.sampleUniform();
    }
    double direction = Rcpp::runif(1, 0, PI_DOUBLE)[0];
    std::vector<Polygon> polygonSplit;
    if (currentPolygon.isConvex())
    {
      polygonSplit = splitPolygon(currentPolygon, cutPoint, direction);
    }
    else
    {
      polygonSplit = splitPolygonRecursive(currentPolygon, cutPoint, direction);
    }

    newCut.addPolygons(polygonSplit);
  }
  return newCut;
}

Cut createCut(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector group)
{
  int n = x.length();
  std::vector<Point> points;
  std::vector<Polygon> polygons;
  int prevGroup = group[0];
  for (int i = 0; i < n; i++)
  {
    if (group[i] != prevGroup)
    {
      polygons.push_back(Polygon(points));
      points.clear();
    }
    points.push_back(Point(x[i], y[i]));
    prevGroup = group[i];
  }
  polygons.push_back(Polygon(points));
  Cut output = Cut(polygons);
  return output;
}

// [[Rcpp::export]]

Rcpp::DataFrame cutEveryPolygonRcpp(Rcpp::NumericVector x, Rcpp::NumericVector y,
                                    Rcpp::NumericVector group, int numberOfIterations = 1, bool useCentroid = true)
{
  Cut cut = createCut(x, y, group);
  for (int k = 0; k < numberOfIterations; k++)
  {
    cut = cutEveryPolygon(cut, useCentroid);
  }

  int nRows = cut.getTotalNumberOfPoints();
  std::vector<double> xVector, yVector;
  std::vector<int> groupVector;
  xVector.reserve(nRows);
  yVector.reserve(nRows);
  groupVector.reserve(nRows);

  for (int i = 0; i < cut.getNumberOfPolygons(); i++)
  {
    Polygon polygon = cut.getPolygon(i);
    std::vector<Point> points = polygon.getPoints();
    for (int j = 0; j < points.size(); j++)
    {
      xVector.push_back(points[j].x);
      yVector.push_back(points[j].y);
      groupVector.push_back(i + 1);
    }
  }
  Rcpp::DataFrame out = Rcpp::DataFrame::create(Rcpp::Named("x") = xVector,
                                                Rcpp::Named("y") = yVector,
                                                Rcpp::Named("group") = groupVector);
  return out;
}

// [[Rcpp::export]]

Rcpp::DataFrame cutEveryPolygonReturnAllRcpp(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector group,
                                             int numberOfIterations = 1, bool useCentroid = true)
{
  Cut cut = createCut(x, y, group);
  int nRows = cut.getTotalNumberOfPoints();

  std::vector<Cut> cuts;
  cuts.reserve(numberOfIterations + 1);
  cuts.push_back(cut);

  for (int k = 0; k < numberOfIterations; k++)
  {
    cut = cutEveryPolygon(cut, useCentroid);
    nRows += cut.getTotalNumberOfPoints();
    cuts.push_back(cut);
  }

  std::vector<double> xVector, yVector;
  std::vector<int> groupVector, iterationVector;
  for (int k = 0; k < cuts.size(); k++)
  {
    Cut currentCut = cuts[k];
    for (int i = 0; i < currentCut.getNumberOfPolygons(); i++)
    {
      Polygon polygon = currentCut.getPolygon(i);
      std::vector<Point> points = polygon.getPoints();
      for (int j = 0; j < points.size(); j++)
      {
        xVector.push_back(points[j].x);
        yVector.push_back(points[j].y);
        groupVector.push_back(i + 1);
        iterationVector.push_back(k);
      }
    }
  }

  Rcpp::DataFrame out = Rcpp::DataFrame::create(Rcpp::Named("x") = xVector,
                                                Rcpp::Named("y") = yVector,
                                                Rcpp::Named("group") = groupVector,
                                                Rcpp::Named("iteration") = iterationVector);
  return out;
}
