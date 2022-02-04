import argparse
import ast
from custom_types import Parameters
from cmv import create_cmv
from pum import create_pum
from fuv import create_fuv
from launch import launch


def __main__(args):
    points = ast.literal_eval(args.points[0])
    X = [x for [x, _] in points]
    Y = [y for [_, y] in points]
    params = \
        Parameters(
            X, Y, args.num_points[0], args.length1[0], args.radius1[0], args.epsilon[0], args.area1[0],
            args.q_pts[0], args.quads[0], args.dist[0], args.n_pts[0], args.k_pts[0], args.a_pts[0], args.b_pts[0],
            args.c_pts[0], args.d_pts[0], args.e_pts[0], args.f_pts[0], args.g_pts[0], args.length2[0],
            args.radius2[0], args.area2[0]
        )

    cmv = create_cmv(params)
    lcm = ast.literal_eval(args.lcm[0])
    pum = create_pum(lcm, cmv)
    puv = ast.literal_eval(args.puv[0])
    fuv = create_fuv(puv, pum)

    did_launch = launch(fuv)

    print("\n====================== 👨‍⚖️ DECIDE 👨‍⚖️ ======================\n")
    if did_launch:
        print("DECIDE: interceptor 🚀 UNLOCKED 🚀, launch APPROVED!")
    else:
        print("DECIDE: interceptor 🔒 LOCKED 🔒, do NOT launch!")
    print("\n==========================================================\n")

    return 0


if __name__ == "__main__":
    CLI = argparse.ArgumentParser()
    CLI.add_argument(
        "--num-points",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--points",
        nargs=1,
        type=str,
        required=True,
    )
    CLI.add_argument(
        "--lcm",
        nargs=1,
        type=str,
        required=True,
    )
    CLI.add_argument(
        "--puv",
        nargs=1,
        type=str,
        required=True,
    )
    CLI.add_argument(
        "--dist",
        nargs=1,
        type=float,
        required=True,
    )
    CLI.add_argument(
        "--quads",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--length1",
        nargs=1,
        type=float,
        required=True,
    )
    CLI.add_argument(
        "--length2",
        nargs=1,
        type=float,
        required=True,
    )
    CLI.add_argument(
        "--radius1",
        nargs=1,
        type=float,
        required=True,
    )
    CLI.add_argument(
        "--radius2",
        nargs=1,
        type=float,
        required=True,
    )
    CLI.add_argument(
        "--area1",
        nargs=1,
        type=float,
        required=True,
    )
    CLI.add_argument(
        "--area2",
        nargs=1,
        type=float,
        required=True,
    )
    CLI.add_argument(
        "--epsilon",
        nargs=1,
        type=float,
        required=True,
    )
    CLI.add_argument(
        "--q-pts",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--n-pts",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--k-pts",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--a-pts",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--b-pts",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--c-pts",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--d-pts",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--e-pts",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--f-pts",
        nargs=1,
        type=int,
        required=True,
    )
    CLI.add_argument(
        "--g-pts",
        nargs=1,
        type=int,
        required=True,
    )
    __main__(CLI.parse_args())
