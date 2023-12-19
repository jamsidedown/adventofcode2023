namespace StuckOnDayTwelve;

public class Program
{
    public static void Main()
    {
        Console.WriteLine(Directory.GetCurrentDirectory());
        var lines = File.ReadAllLines("../../input/day12.txt");
        Console.WriteLine($"Part one: {PartOne(lines)}");
        Console.WriteLine($"Part two: {PartTwo(lines)}");
    }

    private static long PartOne(string[] lines)
    {
        var counts = lines
            .Select(line => ConditionRecord.Parse(line, 1))
            .Select(rec => CountMatches(rec.Springs.AsSpan(), rec.Groups))
            .ToArray();
        return counts.Sum();
    }

    private static long PartTwo(string[] lines)
    {
        return lines
            .Select(line => ConditionRecord.Parse(line, 5))
            .AsParallel()
            .Select(rec => CountMatches(rec.Springs.AsSpan(), rec.Groups))
            .Sum();
    }

    private static long CountMatches(ReadOnlySpan<Condition> template, int[] groups)
    {
        if (template.Length == 0)
            return groups.Length > 0 ? 0L : 1L;

        if (groups.Length == 0)
        {
            foreach (var condition in template)
            {
                if (condition == Condition.Damaged)
                    return 0L;
            }

            return 1L;
        }

        var middleIndex = groups.Length / 2;
        var middleGroup = groups[middleIndex];

        var leftGroups = groups[..middleIndex];
        var rightGroups = groups[(middleIndex + 1)..];

        var leftBuffer = leftGroups.Sum() + leftGroups.Length;
        var rightBuffer = rightGroups.Sum() + rightGroups.Length;
        
        var count = 0L;

        var stop = template.Length - rightBuffer - middleGroup;
        for (var start = leftBuffer; start <= stop; start++)
        {
            var end = start + middleGroup;
            var sub = template[start..end];

            if (!NoneOperational(sub))
                continue;

            if (start > 0 && template[start - 1] == Condition.Damaged)
                continue;

            if (end < template.Length && template[end] == Condition.Damaged)
                continue;

            var leftSprings = template[..(start > 0 ? start - 1 : 0)];
            var rightSprings = template[(end < (template.Length - 1) ? end + 1 : end)..];
            var leftCount = CountMatches(leftSprings, leftGroups);

            if (leftCount == 0L)
                continue;

            var rightCount = CountMatches(rightSprings, rightGroups);
            count += leftCount * rightCount;
        }

        return count;
    }

    private static bool NoneOperational(ReadOnlySpan<Condition> springs)
    {
        foreach (var spring in springs)
        {
            if (spring == Condition.Operational)
                return false;
        }
    
        return true;
    }
}

enum Condition : byte
{
    Operational,
    Damaged,
    Unknown
}

class ConditionRecord
{
    public required Condition[] Springs;
    public required int[] Groups;

    public static ConditionRecord Parse(string line, int repeat)
    {
        var parts = line.Split(' ');
        
        var springs = Repeat(parts[0], repeat, '?')
            .Select(c =>
            {
                return c switch
                {
                    '.' => Condition.Operational,
                    '#' => Condition.Damaged,
                    _ => Condition.Unknown
                };
            }).ToArray();

        var groups = Repeat(parts[1], repeat, ',')
            .Split(',').Select(int.Parse).ToArray();

        return new ConditionRecord
        {
            Springs = springs,
            Groups = groups
        };
    }
    
    private static string Repeat(string s, int times, char connector)
    {
        return string.Join(connector, Enumerable.Repeat(s, times));
    }
}
