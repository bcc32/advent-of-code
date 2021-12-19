def simulate(vx, vy, xmin, xmax, ymin, ymax)
  x, y = 0, 0
  max_y_value = y
  while x <= xmax && y >= ymin
    return max_y_value if (xmin..xmax) === x && (ymin..ymax) === y

    x += vx
    y += vy

    max_y_value = y if y > max_y_value

    if vx > 0
      vx -= 1
    elsif vx < 0
      vx += 1
    else
      vx = 0
    end

    vy -= 1
  end

  nil
end
