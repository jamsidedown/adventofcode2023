using System.Diagnostics;
using CommunityToolkit.HighPerformance;
using CommunityToolkit.HighPerformance.Enumerables;

namespace DayFourteen;

public class Program
{
    public static void Main()
    {
        var lines = File.ReadAllLines("../../input/day14.txt");
        Console.WriteLine($"Part one: {PartOne(lines)}");
        Console.WriteLine($"Part two: {PartTwo(lines)}");
    }

    private static long PartOne(string[] lines)
    {
        var platform = Platform.Parse(lines);
        Platform.Shift(platform, Direction.North);

        return Platform.Score(platform);
    }

    private static long PartTwo(string[] lines)
    {
        var platform = Platform.Parse(lines);

        var cache = new Dictionary<string, int>();
        var limit = 1_000_000_000;
        
        for (var i = 0; i < limit; i++)
        {
            Platform.Cycle(platform);
            
            var s = Platform.AsString(platform);
            if (cache.TryGetValue(s, out var iter))
            {
                var cycle = i - iter;
                var remaining = limit - i;
                var reduced = remaining % cycle;
                i = limit - reduced;
                cache.Clear();
            }
            else
            {
                cache.Add(s, i);
            }
        }

        return Platform.Score(platform);
    }
}

internal static class Platform
{
    public static void Print(Span2D<Tile> platform)
    {
        for (var row = 0; row < platform.Height; row++)
        {
            var line = new char[platform.Width];
            for (var col = 0; col < platform.Width; col++)
            {
                line[col] = platform[row, col] switch
                {
                    Tile.Round => 'O',
                    Tile.Cube => '#',
                    _ => '.'
                };
            }
            Console.WriteLine(new string(line));
        }
    }

    public static string AsString(Span2D<Tile> state)
    {
        var i = 0;
        byte buffer = 0;

        var chars = new List<char>((state.Width * state.Height / 4) + 1);
        
        for (var y = 0; y < state.Height; y++)
        {
            for (var x = 0; x < state.Width; x++)
            {
                if (i % 4 == 0 && i > 0)
                {
                    chars.Add(ToHex(buffer));
                    buffer = 0;
                }

                if (state[x, y] == Tile.Round)
                    buffer += (byte)(1 << (i % 4));
                
                i++;
            }
        }

        if (i % 4 != 0)
            chars.Add(ToHex(buffer));

        return new string(chars.ToArray());
    }

    private static char ToHex(byte value) =>
        value switch
        {
            <= 9 => (char)('0' + value),
            <= 15 => (char)('a' + (value - 10)),
            _ => throw new ArgumentException("Value must be between 0 and 15 inclusive")
        };

    public static Span2D<Tile> Parse(string[] lines)
    {
        var height = lines.Length;
        var width = lines[0].Length;
        var tiles = new Tile[width, height];

        for (var y = 0; y < height; y++)
        {
            var line = lines[y];
            for (var x = 0; x < width; x++)
            {
                var tile = line[x] switch
                {
                    'O' => Tile.Round,
                    '#' => Tile.Cube,
                    _ => Tile.Empty
                };

                tiles[y, x] = tile;
            }
        }

        return new Span2D<Tile>(tiles);
    }

    public static void ShiftLeft(Span<Tile> tiles)
    {
        var roundCount = 0;
        foreach (var tile in tiles)
        {
            if (tile == Tile.Round)
                roundCount++;
        }
        
        for (var i = 0; i < tiles.Length; i++)
        {
            tiles[i] = i < roundCount ? Tile.Round : Tile.Empty;
        }
    }

    public static void ShiftRight(Span<Tile> tiles)
    {
        var roundCount = 0;
        foreach (var tile in tiles)
        {
            if (tile == Tile.Round)
                roundCount++;
        }
        
        for (var i = 0; i < tiles.Length; i++)
        {
            tiles[^(i + 1)] = i < roundCount ? Tile.Round : Tile.Empty;
        }
    }

    public static void ShiftRow(Span<Tile> tiles, Side side)
    {
        var start = 0;
        for (var i = 0; i < tiles.Length; i++)
        {
            if (tiles[i] == Tile.Cube)
            {
                var sub = tiles[start..i];
                if (side == Side.Left)
                    ShiftLeft(sub);
                else
                    ShiftRight(sub);
                start = i + 1;
            }
        }

        if (start < tiles.Length)
        {
            var sub = tiles[start..];
            if (side == Side.Left)
                ShiftLeft(sub);
            else
                ShiftRight(sub);
        }
    }

    public static void ShiftRow(RefEnumerable<Tile> row, Side side)
    {
        var tiles = row.ToArray().AsSpan();
        
        ShiftRow(tiles, side);

        row.TryCopyFrom(tiles);
    }
    
    public static void Shift(Span2D<Tile> tiles, Direction dir)
    {
        if (dir == Direction.North || dir == Direction.South)
        {
            for (var col = 0; col < tiles.Width; col++)
            {
                var tileCol = tiles.GetColumn(col);
                var side = dir == Direction.North ? Side.Left : Side.Right;
                ShiftRow(tileCol, side);
            }
        }
        else
        {
            for (var row = 0; row < tiles.Width; row++)
            {
                var tileRow = tiles.GetRowSpan(row);
                var side = dir == Direction.East ? Side.Right : Side.Left;
                ShiftRow(tileRow, side);
            }
        }
    }

    public static void Cycle(Span2D<Tile> tiles)
    {
        Shift(tiles, Direction.North);
        Shift(tiles, Direction.West);
        Shift(tiles, Direction.South);
        Shift(tiles, Direction.East);
    }

    public static long ScoreRow(RefEnumerable<Tile> row)
    {
        var score = 0L;
        var length = row.Length;
        
        for (var i = 0; i < length; i++)
        {
            if (row[i] == Tile.Round)
                score += length - i;
        }
        
        return score;
    }

    public static long Score(Span2D<Tile> tiles)
    {
        var score = 0L;

        for (var col = 0; col < tiles.Width; col++)
            score += ScoreRow(tiles.GetColumn(col));
        
        return score;
    }
}

enum Direction : byte
{
    North,
    East,
    South,
    West
}

enum Side : byte
{
    Left,
    Right
}

enum Tile : byte
{
    Round,
    Cube,
    Empty
}