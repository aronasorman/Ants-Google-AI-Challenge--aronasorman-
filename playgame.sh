TOOLS_DIR=../ant_tools
python $TOOLS_DIR/playgame.py "sh profBot" "python $TOOLS_DIR/sample_bots/python/HunterBot.py" --map_file $TOOLS_DIR/maps/example/tutorial1.map --log_dir game_logs --turns 60 --scenario --player_seed 7 --food none --verbose -e
