#include "CDT/CDTUtils.h"
#include "CDT/Triangulation.h"
#include <cstdint>

struct Point {
  double x;
  double y;
};

static_assert(sizeof(CDT::VertInd) == 4);

struct Edge {
  std::uint32_t start;
  std::uint32_t end;
};

extern "C" {
  void
  opensolid_cdt(
    std::uint32_t inputPointCount,
    const Point* inputPointData,
    std::uint32_t inputEdgeCount,
    const Edge* inputEdgeData,
    std::uint32_t* outputTriangleCount,
    std::array<std::uint32_t, 3>* outputTriangleData
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
    std::uint32_t triangleCount = cdt.triangles.size();
    *outputTriangleCount = triangleCount;
    for (std::uint32_t i = 0; i < triangleCount; ++i) {
      outputTriangleData[i] = cdt.triangles[i].vertices;
    }
  }
}
