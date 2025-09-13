#include "HsFFI.h"

#include "CDT/CDTUtils.h"
#include "CDT/Triangulation.h"

struct Point {
  double x;
  double y;
};

static_assert(sizeof(CDT::VertInd) == 4);

struct Edge {
  HsInt start;
  HsInt end;
};

extern "C" {
  void
  opensolid_cdt(
    HsInt inputPointCount,
    const Point* inputPointData,
    HsInt inputEdgeCount,
    const Edge* inputEdgeData,
    HsInt* outputTriangleCount,
    std::array<HsInt, 3>* outputTriangleData
  ) {
    CDT::Triangulation<double> cdt;
    cdt.insertVertices(
      inputPointData,
      inputPointData + inputPointCount,
      [](const Point& point) {
        return point.x;
      },
      [](const Point& point) {
        return point.y;
      }
    );
    cdt.insertEdges(
      inputEdgeData,
      inputEdgeData + inputEdgeCount,
      [](const Edge& edge) {
        return edge.start;
      },
      [](const Edge& edge) {
        return edge.end;
      }
    );
    cdt.eraseOuterTrianglesAndHoles();
    HsInt triangleCount = cdt.triangles.size();
    *outputTriangleCount = triangleCount;
    for (HsInt i = 0; i < triangleCount; ++i) {
      auto vertices = cdt.triangles[i].vertices;
      outputTriangleData[i][0] = vertices[0];
      outputTriangleData[i][1] = vertices[1];
      outputTriangleData[i][2] = vertices[2];
    }
  }
}
