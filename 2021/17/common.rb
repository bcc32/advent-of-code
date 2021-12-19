def simulate(vx, vy, xmin, xmax, ymin, ymax, highest_y)
  x, y = 0, 0
  while x <= xmax && y >= ymin
    return true if (xmin..xmax) === x && (ymin..ymax) === y

    x += vx
    y += vy

    highest_y[0] = [highest_y[0], y].max

    if vx > 0
      vx -= 1
    elsif vx < 0
      vx += 1
    else
      vx = 0
    end

    vy -= 1
  end

  false
end
